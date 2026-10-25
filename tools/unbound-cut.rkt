#lang racket

;; effervesce/tools/unbound-cut.rkt
;;
;; Classifies every `unbound` receiver (a bare name not assigned in the body)
;; from the whole-program unknown tally into the resolution lever it belongs to:
;;   'import   — name bound by an import (needs a module-path effect-domain tag)
;;   'global   — name assigned at module level (needs module-global tracking)
;;   'closure  — name is a parameter of a lexically enclosing function
;;   'builtin  — Python builtin object used as a receiver (object/type/...)
;;   'other
;; Reports the split plus the actionable lists: import targets to tag, and
;; global names to track.
;;
;; Usage:
;;   racket tools/unbound-cut.rkt <dir> [<dir> ...] [--top N]

(require racklr/uir
         racklr/effects/infer
         racklr/effects/contract
         racklr/effects/imports
         "../check/host-python.rkt")

(define (find-py-files dir)
  (define out '())
  (let loop ([d dir])
    (for ([e (in-list (directory-list d))])
      (define p (build-path d e))
      (cond
        [(directory-exists? p) (loop p)]
        [(regexp-match? #rx"[.]py$" e) (set! out (cons p out))]
        [else (void)])))
  (reverse out))

(define (package-root-name root-dir)
  (define comps '())
  (let loop ([d root-dir])
    (define-values (base name dir?) (split-path d))
    (cond
      [(and base (file-exists? (build-path d "__init__.py")))
       (set! comps (cons (path->string name) comps))
       (loop base)]
      [else
       (if (null? comps)
           (let-values ([(b n dir?) (split-path root-dir)]) (path->string n))
           (string-join comps "."))])))

;; Python builtins that appear as receivers with methods.
(define builtin-receivers
  '(object type str int float bool bytes bytearray complex list dict set tuple
    frozenset memoryview range enumerate zip map filter iter next super
    property staticmethod classmethod slice))

;; Module-level names (globals) assigned/defined at top level of a program.
(define (module-global-names program)
  (define names (set))
  (define stmts (if (uir-block? program) (uir-block-stmts program) (list program)))
  (for ([s (in-list stmts)])
    (cond
      [(uir-set!? s)
       (define v (and (uir-var? (uir-set!-name s))
                      (string->symbol (uir-symbol-name (uir-var-name (uir-set!-name s))))))
       (when v (set! names (set-add names v)))]
      [(uir-ann-set!? s)
       (define v (and (uir-var? (uir-ann-set!-lhs s))
                      (string->symbol (uir-symbol-name (uir-var-name (uir-ann-set!-lhs s))))))
       (when v (set! names (set-add names v)))]
      [(uir-class? s)
       (define c (and (uir-class-name s) (uir-symbol? (uir-class-name s))
                      (string->symbol (uir-symbol-name (uir-class-name s)))))
       (when c (set! names (set-add names c)))]
      [(uir-fn? s)
       (define f (and (uir-fn-name s) (uir-symbol? (uir-fn-name s))
                      (string->symbol (uir-symbol-name (uir-fn-name s)))))
       (when f (set! names (set-add names f)))]
      [else (void)]))
  names)

;; For each named function/method, the set of parameter names of lexically
;; enclosing functions (closure captures). Keyed by the bare name symbol.
(define (closure-params program)
  (define h (make-hash))
  (define stack '())
  (define (ps-of params)
    (for/set ([p (in-list params)]
              #:when (and (uir-typed-param? p)
                          (uir-symbol? (uir-typed-param-name p))))
      (string->symbol (uir-symbol-name (uir-typed-param-name p)))))
  (define (union-stack)
    (if (null? stack) (set) (apply set-union stack)))
  (let walk ([n program])
    (cond
      [(uir-fn? n)
       (define nm (and (uir-fn-name n) (uir-symbol? (uir-fn-name n))
                       (string->symbol (uir-symbol-name (uir-fn-name n)))))
       (when nm (hash-set! h nm (union-stack)))
       (set! stack (cons (ps-of (uir-fn-params n)) stack))
       (walk (uir-fn-body n))
       (set! stack (cdr stack))]
      [(uir-method? n)
       (define nm (and (uir-method-name n) (uir-symbol? (uir-method-name n))
                       (string->symbol (uir-symbol-name (uir-method-name n)))))
       (when nm (hash-set! h nm (union-stack)))
       (set! stack (cons (ps-of (uir-method-params n)) stack))
       (walk (uir-method-body n))
       (set! stack (cdr stack))]
      [(uir-class? n)
       (for ([m (in-list (uir-class-methods n))]) (walk m))
       (for ([f (in-list (uir-class-fields n))]) (walk f))]
      [(struct? n)
       (for ([f (in-list (cdr (vector->list (struct->vector n))))]) (walk f))]
      [(pair? n) (walk (car n)) (walk (cdr n))]
      [else (void)]))
  h)

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define top-n
    (or (for/or ([a (in-list args)] [i (in-naturals)] #:when (string=? a "--top"))
          (and (< (add1 i) (length args)) (string->number (list-ref args (add1 i)))))
        30))
  (define dirs
    (let loop ([as args] [acc '()])
      (cond
        [(null? as) (reverse acc)]
        [(string=? (car as) "--top")
         (loop (if (pair? (cdr as)) (cddr as) (cdr as)) acc)]
        [(string-prefix? (car as) "--") (loop (cdr as) acc)]
        [else (loop (cdr as) (cons (car as) acc))])))
  (unless (pair? dirs)
    (eprintf "usage: unbound-cut.rkt <dir> [<dir> ...] [--top N]\n")
    (exit 1))

  (define roots
    (for/list ([dir (in-list dirs)])
      (define root-dir (simplify-path (path->complete-path dir) #f))
      (cons (package-root-name root-dir) root-dir)))
  (define files
    (sort (apply append (map (λ (r) (find-py-files (cdr r))) roots))
          (λ (a b) (string<? (path->string a) (path->string b)))))
  (define host-code (make-hash))
  (define failed '())
  (define specs '())
  (define packages (set))
  (define comp-modpath (make-hash))
  (define import-targets (make-hash))   ;; comp -> (hashof local-name-str -> target-str)
  (define global-names (make-hash))     ;; comp -> (setof name)
  (define closure-tables (make-hash))   ;; comp -> (hashof fnname -> (setof param-name))

  (for ([f (in-list files)])
    (define src (file->string f))
    (define uir (python-source->uir src))
    (cond
      [uir (hash-set! host-code (path->string f) uir)]
      [else (set! failed (cons f failed))])
    (define modpath
      (for/or ([r (in-list roots)])
        (define rn (car r))
        (define rd (cdr r))
        (define rel (find-relative-path rd f))
        (and (not (string-prefix? (path->string rel) ".."))
             (path->module-path rn rd f))))
    (when modpath
      (define comp (path->string f))
      (hash-set! comp-modpath comp modpath)
      (when (regexp-match? #rx"__init__[.]py$" comp)
        (set! packages (set-add packages modpath)))
      (define tks (for/list ([t (in-list (py-tokenize src))])
                    (cons (py-tok-type t) (py-tok-value t))))
      (define imps (extract-imports tks))
      (set! specs (cons (module-spec modpath (or uir (uir-block '())) imps) specs))
      ;; local-name -> target, for import classification
      (define tgt (make-hash))
      (for ([r (in-list imps)])
        (for ([b (in-list (import-record-bindings r))])
          (hash-set! tgt (car b) (cdr b))))
      (hash-set! import-targets comp tgt)
      (when uir
        (hash-set! global-names comp (module-global-names uir))
        (hash-set! closure-tables comp (closure-params uir)))))

  (define mods (build-module-table specs))
  (define caps (derive-class-method-effects mods))
  (define supers (derive-class-supers mods))
  (define env (import-env comp-modpath (resolve-exports mods packages) caps))

  (define tally (box '()))
  (define global-rows
    (infer-effects-all (for/list ([(c p) (in-hash host-code)]) (cons c p))
                       #:import-env env #:unknown-tally tally))

  ;; classify unbound tally records
  (define class-counts (make-hash))
  (define import-target-counts (make-hash))
  (define global-name-counts (make-hash))
  (define other-name-counts (make-hash))
  (define total 0)
  (for ([rec (in-list (unbox tally))])
    (when (and (eq? (third rec) 'local)
               (eq? (sixth rec) 'unbound))
      (define comp (first rec))
      (define name (second rec))
      (define rx (fifth rec))
      (define rxstr (and rx (symbol->string rx)))
      (set! total (add1 total))
      (define tgt (and rxstr (hash-ref import-targets comp (hash))
                       (hash-ref (hash-ref import-targets comp (hash)) rxstr #f)))
      (define globals (hash-ref global-names comp (set)))
      (define closures (hash-ref (hash-ref closure-tables comp (hash)) name #f))
      (define cls
        (cond
          [tgt 'import]
          [(and rx (set-member? globals rx)) 'global]
          [(and rx closures (set-member? closures rx)) 'closure]
          [(and rx (memq rx builtin-receivers)) 'builtin]
          [else 'other]))
      (hash-update! class-counts cls add1 0)
      (cond
        [(eq? cls 'import) (hash-update! import-target-counts tgt add1 0)]
        [(eq? cls 'global) (hash-update! global-name-counts rx add1 0)]
        [(eq? cls 'other) (hash-update! other-name-counts rx add1 0)]
        [else (void)])))

  (printf "unbound sites:        ~a\n" total)
  (printf "\nClassification:\n")
  (for ([e (in-list (sort (hash->list class-counts) (λ (a b) (> (cdr a) (cdr b)))))])
    (printf "  ~a  x~a  (~a%)\n" (car e) (cdr e)
            (if (zero? total) 0 (round (* 100 (/ (cdr e) total))))))

  (printf "\nImport targets to tag (module path -> domain):\n")
  (for ([e (in-list (sort (hash->list import-target-counts) (λ (a b) (> (cdr a) (cdr b)))))]
        [i (in-naturals)] #:break (>= i top-n))
    (printf "  ~a  x~a\n" (car e) (cdr e)))

  (printf "\nGlobal names to track:\n")
  (for ([e (in-list (sort (hash->list global-name-counts) (λ (a b) (> (cdr a) (cdr b)))))]
        [i (in-naturals)] #:break (>= i top-n))
    (printf "  ~a  x~a\n" (car e) (cdr e)))

  (printf "\nOther (unclassified) names:\n")
  (for ([e (in-list (sort (hash->list other-name-counts) (λ (a b) (> (cdr a) (cdr b)))))]
        [i (in-naturals)] #:break (>= i top-n))
    (printf "  ~a  x~a\n" (car e) (cdr e))))

(main)

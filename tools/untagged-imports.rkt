#lang racket

;; effervesce/tools/untagged-imports.rkt
;;
;; For every `unknown` call site the whole-program fixpoint produces, resolve
;; the receiver's base name (or bare-callee name) through the import graph and
;; report, per resolved module path, whether a domain rule exists — the exact
;; gap that makes `imported-effect` / `imported-fn-effect` fall through to
;; `unknown`. Aim the next library-config.rkt DATA addition at real modules.

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

(define (main)
  (define dirs (let loop ([as (vector->list (current-command-line-arguments))] [acc '()])
                 (cond
                   [(null? as) (reverse acc)]
                   [(string-prefix? (car as) "--") (loop (cdr as) acc)]
                   [else (loop (cdr as) (cons (car as) acc))])))
  (unless (pair? dirs)
    (eprintf "usage: untagged-imports.rkt <dir> [<dir> ...]\n")
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
  (for ([f (in-list files)])
    (define src (file->string f))
    (define uir (python-source->uir src))
    (cond [uir (hash-set! host-code (path->string f) uir)]
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
      (set! specs (cons (module-spec modpath (or uir (uir-block '()))
                                    (extract-imports tks))
                        specs))))

  (define mods (build-module-table specs))
  (define caps (derive-class-method-effects mods))
  (define supers (derive-class-supers mods))
  (define env
    (import-env comp-modpath
                (resolve-exports mods packages)
                caps))

  (define tally (box '()))
  (infer-effects-all (for/list ([(c p) (in-hash host-code)]) (cons c p))
                     #:import-env env
                     #:unknown-tally tally)

  (define exports (import-env-exports env))
  (define untagged (make-hash))   ;; (kind . path) -> (hash method -> count)
  (define unresolved (make-hash)) ;; name -> count  (no import handle at all)
  (define tagged (make-hash))     ;; (dom . path) -> count
  (define total 0)
  (for ([rec (in-list (unbox tally))])
    (define comp (first rec))
    (define shape (third rec))
    (define op (fourth rec))
    (define rx (fifth rec))
    (set! total (add1 total))
    (define modpath (hash-ref (import-env-comp-modpath env) comp #f))
    (define e (and modpath (hash-ref exports modpath #f)))
    (define name-str
      (cond
        [rx (symbol->string rx)]
        [(eq? shape 'bare-call) (and (symbol? op) (symbol->string op))]
        [else #f]))
    (define h (and e name-str (hash-ref e name-str #f)))
    (cond
      [(and (handle? h) (handle-path h))
       (define hpath (handle-path h))
       (define dom (module-effect-tag hpath))
       (define meth (and (symbol? op) (symbol->string op)))
       (if dom
           (hash-update! tagged (cons dom hpath) add1 0)
           (let ([k (cons (handle-kind h) hpath)])
             (define m (hash-ref! untagged k make-hash))
             (hash-update! m (or meth "?") add1 0)))]
      [else
       (hash-update! unresolved (or name-str "?") add1 0)]))

  (printf "files scanned: ~a  parsed: ~a  failed: ~a\n"
          (length files) (hash-count host-code) (length failed))
  (printf "unknown call sites: ~a\n\n" total)

  (printf "UNTAGGED imported modules (no domain rule -> fall through to unknown):\n")
  (for ([e (in-list (sort (hash->list untagged)
                          (λ (a b) (> (apply + (hash-values (cdr a)))
                                      (apply + (hash-values (cdr b)))))))])
    (printf "  ~a ~a   (n=~a)\n" (car (car e)) (cdr (car e))
            (apply + (hash-values (cdr e))))
    (for ([m (in-list (sort (hash->list (cdr e))
                            (λ (a b) (> (cdr a) (cdr b)))))])
      (printf "      ~a  x~a\n" (car m) (cdr m))))

  (printf "\nUNRESOLVED names (no import handle — globals/closures, not imports):\n")
  (for ([e (in-list (sort (hash->list unresolved)
                          (λ (a b) (> (cdr a) (cdr b)))))])
    (printf "  ~a  x~a\n" (car e) (cdr e)))

  (printf "\nTAGGED (resolved) modules for reference:\n")
  (for ([e (in-list (sort (hash->list tagged)
                          (λ (a b) (> (cdr a) (cdr b)))))])
    (printf "  ~a ~a  x~a\n" (car (car e)) (cdr (car e)) (cdr e))))

(main)

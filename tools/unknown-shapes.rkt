#lang racket

;; effervesce/tools/unknown-shapes.rkt
;;
;; Call-site shape census of every `unknown` the whole-program effect fixpoint
;; produces. Runs the REAL fixpoint (infer-effects-all) with the #:unknown-tally
;; sink, then buckets each unknown call site by receiver shape — the lever that
;; resolves it — and reports the shape histogram plus the dominant method names
;; per shape, so the next `unknown`-reduction lever is aimed at real call sites
;; instead of guessed from the aggregate count.
;;
;; Shapes (see racklr/effects/classify-all.rkt receiver-shape):
;;   bare-call    — foo(...)                    -> DATA-lexicon lever
;;   self-method  — self.m() / cls.m()          -> MRO/edge lever (already done)
;;   self-attr    — self.attr.m()               -> stdlib-constructor/field lever
;;   call-result  — f(...).m()                  -> return-type projection lever
;;   chained      — a.b.m() (non-self root)     -> multi-level chain lever
;;   param        — x.m() where x is a param    -> PARAMETER dataflow lever
;;   local        — x.m() where x is a local    -> local constructor tracking
;;   symbol       — Name.m()                    -> import/lexicon lever
;;   other
;;
;; Usage:
;;   racket tools/unknown-shapes.rkt <dir> [<dir> ...] [--top N]
;;
;; Requires PLTCOLLECTS to include the repo root, as type-graph.rkt does.

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

;; Canonical dotted module name of a scanned root directory (same as
;; type-graph.rkt): walk up past __init__.py ancestors so scanning
;; django/django/core yields "django.core".
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
  (define args (vector->list (current-command-line-arguments)))
  (define top-n
    (or (for/or ([a (in-list args)] [i (in-naturals)]
                #:when (string=? a "--top"))
          (and (< (add1 i) (length args))
               (string->number (list-ref args (add1 i)))))
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
    (eprintf "usage: unknown-shapes.rkt <dir> [<dir> ...] [--top N]\n")
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

  ;; Import-surface material for the fixpoint (same as type-graph.rkt).
  (define specs '())
  (define packages (set))
  (define comp-modpath (make-hash))
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

  ;; Whole-program inference with the unknown tally sink.
  (define tally (box '()))
  (define global-rows
    (infer-effects-all (for/list ([(c p) (in-hash host-code)]) (cons c p))
                       #:import-env env
                       #:unknown-tally tally))

  ;; Unknown function/file counts from the inferred rows.
  (define unknown-fns 0)
  (define unknown-files '())
  (for ([(k row) (in-hash global-rows)])
    (define names
      (map (λ (e) (effect-name-symbol e)) (uir-effect-row-entries row)))
    (when (memq 'unknown names)
      (set! unknown-fns (add1 unknown-fns))
      (unless (member (car k) unknown-files)
        (set! unknown-files (cons (car k) unknown-files)))))

  ;; Aggregate the tally by receiver shape, and by (shape . op).
  (define shape-counts (make-hash))
  (define shape-op-counts (make-hash))
  (define local-kind-counts (make-hash))   ;; binding-kind -> count, for 'local
  (define unbound-op-counts (make-hash))
  (define unbound-rx-counts (make-hash))
  (define total 0)
  (for ([rec (in-list (unbox tally))])
    (define shape (third rec))
    (define op (fourth rec))
    (define rx (fifth rec))
    (define rxk (sixth rec))
    (set! total (add1 total))
    (hash-update! shape-counts shape add1 0)
    (hash-update! shape-op-counts (cons shape op) add1 0)
    (when (eq? shape 'local)
      (hash-update! local-kind-counts rxk add1 0)
      (when (eq? rxk 'unbound)
        (hash-update! unbound-op-counts op add1 0)
        (hash-update! unbound-rx-counts (or rx '?) add1 0))))

  (printf "files scanned:       ~a   parsed: ~a   failed: ~a\n"
          (length files) (hash-count host-code) (length failed))
  (printf "unknown call sites:  ~a\n" total)
  (printf "unknown functions:   ~a\n" unknown-fns)
  (printf "unknown files:       ~a\n" (length unknown-files))

  (printf "\nReceiver-shape histogram (unknown call sites):\n")
  (for ([e (in-list (sort (hash->list shape-counts)
                          (λ (a b) (> (cdr a) (cdr b)))))])
    (printf "  ~a  x~a  (~a%)\n"
            (car e) (cdr e)
            (if (zero? total) 0 (round (* 100 (/ (cdr e) total))))))

  (printf "\n`local` receiver binding-kind breakdown:\n")
  (for ([e (in-list (sort (hash->list local-kind-counts)
                          (λ (a b) (> (cdr a) (cdr b)))))])
    (printf "  ~a  x~a  (~a% of local, ~a% of all)\n"
            (car e) (cdr e)
            (if (zero? (hash-ref shape-counts 'local 0)) 0
                (round (* 100 (/ (cdr e) (hash-ref shape-counts 'local 0)))))
            (if (zero? total) 0 (round (* 100 (/ (cdr e) total))))))

  (printf "\nTop methods per shape (receiver shape -> method):\n")
  (for ([shape '(call-result param chained self-attr self-method bare-call local)])
    (define per-op
      (sort (for/list ([(k c) (in-hash shape-op-counts)]
                       #:when (eq? (car k) shape))
              (cons (cdr k) c))
            (λ (a b) (> (cdr a) (cdr b)))))
    (unless (null? per-op)
      (printf "  ~a (top ~a):\n" shape (min 15 (length per-op)))
      (for ([e (in-list (take per-op 15))])
        (printf "      ~a  x~a\n" (car e) (cdr e)))))

  (printf "\n`unbound` local receivers — top names and methods (imports/globals/closures):\n")
  (printf "  by receiver name:\n")
  (for ([e (in-list (sort (hash->list unbound-rx-counts)
                          (λ (a b) (> (cdr a) (cdr b)))))]
        [i (in-naturals)]
        #:break (>= i 25))
    (printf "      ~a  x~a\n" (car e) (cdr e)))
  (printf "  by method:\n")
  (for ([e (in-list (sort (hash->list unbound-op-counts)
                          (λ (a b) (> (cdr a) (cdr b)))))]
        [i (in-naturals)]
        #:break (>= i 25))
    (printf "      ~a  x~a\n" (car e) (cdr e)))

  (printf "\nPer-shape top methods (the exact unknown operations):\n")
  (for ([shape (in-list (sort (hash-keys shape-counts) symbol<?))])
    (printf "  ~a:\n" shape)
    (define shape-ops
      (sort (for/list ([(k v) (in-hash shape-op-counts)]
                      #:when (eq? (car k) shape))
              (cons (cdr k) v))
            (λ (a b) (> (cdr a) (cdr b)))))
    (for ([e (in-list (take shape-ops (min top-n (length shape-ops))))])
      (printf "      ~a  x~a\n" (car e) (cdr e)))))

(main)

#lang racket

;; effervesce/tools/unknown-sites.rkt
;;
;; Dump every `unknown` call site the whole-program fixpoint produces as one
;; line: file, enclosing function, receiver shape, and the classifying callee
;; site string. Aimed at the exact site instead of the aggregate count — the
;; probe behind the next `unknown`-reduction lever.
;;
;; Usage:
;;   racket tools/unknown-sites.rkt <dir> [<dir> ...] [--shape SHAPE] [--top N]
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
  (define shape-filter
    (for/or ([a (in-list args)] [i (in-naturals)]
            #:when (string=? a "--shape"))
      (and (< (add1 i) (length args))
           (string->symbol (list-ref args (add1 i))))))
  (define top-n
    (or (for/or ([a (in-list args)] [i (in-naturals)]
                #:when (string=? a "--top"))
          (and (< (add1 i) (length args))
               (string->number (list-ref args (add1 i)))))
        999999))
  (define dirs
    (let loop ([as args] [acc '()])
      (cond
        [(null? as) (reverse acc)]
        [(memq (string->symbol (car as)) '(--top --shape))
         (loop (if (pair? (cdr as)) (cddr as) (cdr as)) acc)]
        [(string-prefix? (car as) "--") (loop (cdr as) acc)]
        [else (loop (cdr as) (cons (car as) acc))])))
  (unless (pair? dirs)
    (eprintf "usage: unknown-sites.rkt <dir> [<dir> ...] [--shape SHAPE] [--top N]\n")
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

  (printf "files scanned: ~a  parsed: ~a  failed: ~a  unknown sites: ~a\n"
          (length files) (hash-count host-code) (length failed)
          (length (unbox tally)))
  (define n 0)
  (for ([rec (in-list (reverse (unbox tally)))])
    (define comp (first rec))
    (define fn-name (second rec))
    (define shape (third rec))
    (define op (fourth rec))
    (define rx (fifth rec))
    (define rxk (sixth rec))
    (when (and (or (not shape-filter) (eq? shape shape-filter))
               (< n top-n))
      (set! n (add1 n))
      (printf "~a :: ~a :: ~a :: ~a.~a [rx=~a kind=~a]\n"
              comp fn-name shape (or rx "?") (or op "?") (or rx "?") rxk))))

(main)

#lang racket

;; effervesce/tools/dep-surface.rkt
;;
;; Demonstrates the static import/export resolver over the real Django tree:
;; harvest every module's class/method surface from parsed UIR, resolve the
;; import graph to handles, and derive Class.method → effect by module path.
;; The point is to show that class-granularity tagging (module path → effect)
;; closes the dependency surface instead of hand-listing method names.
;;
;; Usage:
;;   racket tools/dep-surface.rkt <dir> [--top N]
;;
;;   <dir> is the package ROOT (e.g. django/django); its basename is the
;;   top-level package name used to build dotted module paths.

(require racklr/uir
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

(define (path->module-path root-name root-dir p)
  ;; root-dir is the package root; rel is path-relative-to it, minus .py.
  (define rel (path->string (find-relative-path root-dir p)))
  (define comps (map (λ (c) (path->string c))
                     (explode-path (string->path rel))))
  (define comps-no-py
    (let loop ([cs comps])
      (cond [(null? cs) '()]
            [(string=? (car cs) ".") (loop (cdr cs))]
            [else cs])))
  (define base
    (cond
      [(and (pair? comps-no-py) (string=? (car comps-no-py) "__init__.py"))
       (cdr comps-no-py)]
      [else
       (let ([last (last comps-no-py)])
         (append (drop-right comps-no-py 1)
                 (list (regexp-replace #rx"[.]py$" last ""))))]))
  (string-join (cons root-name base) "."))

(define (normalize-tokens tks)
  (for/list ([t (in-list tks)])
    (cons (py-tok-type t) (py-tok-value t))))

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define dir (and (pair? args) (car args)))
  (unless dir
    (eprintf "usage: dep-surface.rkt <package-root>\n")
    (exit 1))
  (define root-name (path->string (file-name-from-path (simplify-path (path->complete-path dir) #f))))
  (define root-dir (simplify-path (path->complete-path dir) #f))

  (define files (sort (find-py-files root-dir)
                      (λ (a b) (string<? (path->string a) (path->string b)))))
  (define specs '())
  (define packages (set))
  (for ([f (in-list files)])
    (define modpath (path->module-path root-name root-dir f))
    (define src (file->string f))
    (define tks (normalize-tokens (py-tokenize src)))
    (define imports (extract-imports tks))
    (define uir (python-source->uir src))
    (define is-pkg? (string-suffix? (path->string f) "__init__.py"))
    (when is-pkg? (set! packages (set-add packages modpath)))
    (set! specs (cons (module-spec modpath uir imports) specs)))
  (define mods (build-module-table specs))
  (define exports (resolve-exports mods packages))
  (define effs (derive-class-method-effects mods))

  (printf "package root:     ~a\n" root-name)
  (printf "modules:          ~a\n" (length specs))
  (printf "packages:         ~a\n" (set-count packages))
  (printf "class-method→effect entries derived: ~a\n" (hash-count effs))

  ;; how many distinct tagged classes, per effect
  (define tagged-classes (make-hash))
  (for ([(path mi) (in-hash mods)])
    (define tag (module-effect-tag path))
    (when tag
      (for ([cname (in-hash-keys (module-info-classes mi))])
        (hash-update! tagged-classes tag add1 0))))
  (printf "tagged classes per effect:\n")
  (for ([(tag n) (in-hash tagged-classes)])
    (printf "  ~a: ~a classes\n" tag n))

  ;; show the dict.get vs QuerySet.get collision resolved by class
  (define qs-methods
    (for/list ([(k v) (in-hash effs)] #:when (string-prefix? k "QuerySet."))
      k))
  (printf "\nQuerySet method surface (derived, ~a methods):\n" (length qs-methods))
  (printf "  ~a\n" (string-join (sort qs-methods string<?) " "))
  (printf "\n'get' collision: dict.get is pure-builtin, QuerySet.get -> ~a\n"
          (hash-ref effs "QuerySet.get" #f))
  (printf "'filter' collision: builtin filter is pure, QuerySet.filter -> ~a\n"
          (hash-ref effs "QuerySet.filter" #f))
  (printf "'save' collision: Model.save -> ~a\n" (hash-ref effs "Model.save" #f))

  ;; unresolved import sources (dependencies we haven't harvested yet)
  (define unresolved (set))
  (for ([(path mi) (in-hash mods)])
    (for ([rec (in-list (module-info-imports mi))])
      (define src (import-record-source rec))
      (when (and (> (string-length src) 0)
                 (not (char=? (string-ref src 0) #\.)))
        (unless (hash-has-key? mods src)
          (set! unresolved (set-add unresolved (first-component src)))))))
  (printf "\nunresolved top-level import roots (deps not harvested): ~a\n"
          (sort (set->list unresolved) string<?)))

(main)

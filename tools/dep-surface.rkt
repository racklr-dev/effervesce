#lang racket

;; effervesce/tools/dep-surface.rkt
;;
;; Demonstrates the static import/export resolver over one or more real Python
;; packages: harvest every module's class/method surface from parsed UIR,
;; resolve the import graph to handles ACROSS package roots (transitively), and
;; derive Class.method → effect from the per-library effect-tag configuration.
;; The point is to show that class-granularity tagging (config data, not code)
;; closes the dependency surface instead of hand-listing method names.
;;
;; Usage:
;;   racket tools/dep-surface.rkt <package-root> [<package-root> ...]
;;
;;   Each <package-root> is a package directory (e.g. django/django, or an
;;   installed asgiref/sqlparse/tzdata site-packages dir); its basename is the
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

(define (normalize-tokens tks)
  (for/list ([t (in-list tks)])
    (cons (py-tok-type t) (py-tok-value t))))

;; Ingest one package root. Returns (values specs packages failed-module-paths).
;; A module that fails to parse still contributes its import edges: its UIR is
;; replaced with an empty block so resolution stays transitive, and its path is
;; recorded in `failed`.
(define (root-specs root-name root-dir)
  (define files (sort (find-py-files root-dir)
                      (λ (a b) (string<? (path->string a) (path->string b)))))
  (define specs '())
  (define packages (set))
  (define failed '())
  (for ([f (in-list files)])
    (define modpath (path->module-path root-name root-dir f))
    (define src (file->string f))
    (define tks (normalize-tokens (py-tokenize src)))
    (define imports (extract-imports tks))
    (define uir (python-source->uir src))
    (when (not uir) (set! failed (cons modpath failed)))
    (define effective-uir (or uir (uir-block '())))
    (define is-pkg? (string-suffix? (path->string f) "__init__.py"))
    (when is-pkg? (set! packages (set-add packages modpath)))
    (set! specs (cons (module-spec modpath effective-uir imports) specs)))
  (values specs packages failed))

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (unless (pair? args)
    (eprintf "usage: dep-surface.rkt <package-root> [<package-root> ...]\n")
    (exit 1))
  (define roots
    (for/list ([dir (in-list args)])
      (define root-dir (simplify-path (path->complete-path dir) #f))
      (define root-name (path->string (file-name-from-path root-dir)))
      (cons root-name root-dir)))

  (define specs '())
  (define packages (set))
  (define failed '())
  (define per-root-counts '())
  (for ([r (in-list roots)])
    (define-values (r-specs r-pkgs r-failed) (root-specs (car r) (cdr r)))
    (set! per-root-counts (cons (cons (car r) (length r-specs)) per-root-counts))
    (set! specs (append specs r-specs))
    (set! packages (set-union packages r-pkgs))
    (set! failed (append failed r-failed)))

  (define mods (build-module-table specs))
  (define exports (resolve-exports mods packages))
  (define effs (derive-class-method-effects mods))

  (printf "roots:\n")
  (for ([rc (in-list (reverse per-root-counts))])
    (printf "  ~a: ~a modules\n" (car rc) (cdr rc)))
  (printf "total modules:    ~a\n" (length specs))
  (printf "packages:         ~a\n" (set-count packages))
  (printf "parse-failed:     ~a\n" (length failed))
  (when (pair? failed)
    (printf "  (first failures): ~s\n" (take failed (min 5 (length failed)))))
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

  ;; show the dict.get vs QuerySet.get collision resolved by class, now at
  ;; operation level: same method name, different class, different (domain ops).
  (define qs-methods
    (for/list ([(k v) (in-hash effs)] #:when (string-prefix? k "QuerySet."))
      k))
  (printf "\nQuerySet method surface (derived, ~a methods):\n" (length qs-methods))
  (printf "  ~a\n" (string-join (sort qs-methods string<?) " "))
  (define (show-cap c)
    (if (effect-cap? c)
        (format "~a ~a" (effect-cap-domain c) (effect-cap-ops c))
        "#f"))
  (printf "\noperation-level effects (domain ops):\n")
  (printf "  QuerySet.get     -> ~a\n" (show-cap (hash-ref effs "QuerySet.get" #f)))
  (printf "  QuerySet.filter  -> ~a\n" (show-cap (hash-ref effs "QuerySet.filter" #f)))
  (printf "  QuerySet.update  -> ~a\n" (show-cap (hash-ref effs "QuerySet.update" #f)))
  (printf "  Model.save       -> ~a\n" (show-cap (hash-ref effs "Model.save" #f)))
  (printf "  File.close       -> ~a\n" (show-cap (hash-ref effs "File.close" #f)))
  (printf "  ContentFile.write -> ~a\n" (show-cap (hash-ref effs "ContentFile.write" #f)))
  (printf "  HttpResponse.write -> ~a\n" (show-cap (hash-ref effs "HttpResponse.write" #f)))
  (printf "  OutputWrapper.write -> ~a\n" (show-cap (hash-ref effs "OutputWrapper.write" #f)))

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

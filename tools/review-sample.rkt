#lang racket

;; effervesce/tools/review-sample.rkt
;;
;; Batch scanner for surfacing off-corpus classifier biases. For each directory
;; given, it sub-samples up to --max-files Python files (path-sorted, size-capped),
;; runs the REAL absorb + whole-program effect inference, and prints:
;;
;;   - parse success/failure (absorb robustness)
;;   - effect histogram (which hardcoded domains fire)
;;   - unknown function/file counts (fixpoint coverage)
;;   - an ORM-collision probe: member-call sites whose verb is in the Django
;;     ORM verb set, with the top (root . verb) pairs — the db-bias detector,
;;     independent of the classifier's exclusion list
;;   - optional --verify: run the four effervesce checkers with a minimal
;;     Cache spec to show how inference bias leaks into a false bridge finding
;;
;; Usage:
;;   racket effervesce/tools/review-sample.rkt <dir> [<dir> ...]
;;         [--max-files N] [--max-size KB] [--verify] [--top N]

(require racket/file
         racket/path
         racklr/uir
         racklr/effects/infer
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/walk
         racklr/effects/shape
         "../check/host-python.rkt"
         "../load.rkt"
         "../check/verify.rkt")

;; The ORM method-name set, mirrored from racklr/effects/library-tokens.rkt so
;; the probe measures the classifier's collision surface without importing the
;; classifier's own tables.
(define orm-verb-set
  (list->set
   (string-split
    "all annotate aggregate alias as_manager count dates datetimes db_manager defer difference distinct earliest exclude exists explain extra filter first get in_bulk intersection iterator last latest none only order_by prefetch_related raw reverse select_related union using values values_list bulk_create bulk_update create delete get_or_create update update_or_create save refresh_from_db execute executemany fetchone fetchall fetchmany commit rollback cursor create_function create_aggregate create_deterministic_function db_for_read db_for_write")))

;; ---- sampling walk ---------------------------------------------------

(define (find-py-files dir max-files max-size)
  (define out '())
  (let loop ([d dir])
    (for ([e (in-list (directory-list d))])
      (define p (build-path d e))
      (cond
        [(directory-exists? p) (loop p)]
        [(and (regexp-match? #rx"[.]py$" e)
              (<= (file-size p) max-size))
         (set! out (cons p out))]
        [else (void)])))
  (take (sort out (λ (a b) (string<? (path->string a) (path->string b))))
        (min max-files (length out))))

;; Canonical dotted package name (same as type-graph.rkt / unknown-shapes.rkt).
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

(define (sym-name x)
  (cond [(uir-symbol? x) (uir-symbol-name x)]
        [(uir-var? x) (sym-name (uir-var-name x))]
        [else #f]))

;; ---- ORM-collision probe ---------------------------------------------

;; Member-call sites whose verb is an ORM verb: the exact call shape
;; orm-callee? keys on, before any receiver-root exclusion. Returns (root . verb)
;; conses with both as strings.
(define (orm-collision-sites program)
  (define sites '())
  (walk-uir
   program
   (λ (n)
     (when (uir-call? n)
       (define c (uir-call-callee n))
       (when (and (member-access-callee? c)
                  (effect-op-of-callee c)
                  (set-member? orm-verb-set (symbol->string (effect-op-of-callee c))))
         (set! sites
               (cons (cons (and (dotted-root c) (symbol->string (dotted-root c)))
                           (symbol->string (effect-op-of-callee c)))
                     sites))))))
  sites)

;; ---- effect histogram -------------------------------------------------

(define (effect-histogram rows)
  (define fn-counts (make-hash))
  (define file-counts (make-hash))
  (for ([(k row) (in-hash rows)])
    (for ([e (in-list (uir-effect-row-entries row))])
      (define nm (effect-name-symbol e))
      (hash-update! fn-counts nm add1 0)
      (hash-update! file-counts nm (λ (s) (set-add s (car k))) (set))))
  (sort (for/list ([nm (in-list (hash-keys fn-counts))])
          (list nm (hash-ref fn-counts nm) (set-count (hash-ref file-counts nm))))
        (λ (a b) (> (second a) (second b)))))

(define (merge-programs programs)
  (uir-block (apply append (map uir-block-stmts programs))))

(define (top-pairs pairs n)
  (define counts (make-hash))
  (for ([p (in-list pairs)])
    (hash-update! counts p add1 0))
  (take (sort (hash->list counts) (λ (a b) (> (cdr a) (cdr b))))
        (min n (hash-count counts))))

(define (hist-str hist)
  (string-join (map (λ (e) (format "~a:~a/~a" (first e) (second e) (third e))) hist) " "))

(define (pairs-str pairs)
  (string-join (map (λ (p) (format "~a.~a" (or (car p) "?") (cdr p))) pairs) " "))

(define (shapes-str shape-counts)
  (string-join
   (map (λ (e) (format "~a:~a" (car e) (cdr e)))
        (sort (hash->list shape-counts) (λ (a b) (> (cdr a) (cdr b)))))
   " "))

;; ---- one directory ----------------------------------------------------

(define (scan-dir root-dir opts)
  (define root-name (package-root-name root-dir))
  (define files (find-py-files root-dir
                               (hash-ref opts 'max-files 15)
                               (* 1024 (hash-ref opts 'max-size 200))))
  (if (null? files)
      (printf "~a\tno .py files under cap\n" root-name)
      (scan-files root-dir opts files root-name))
  (list root-name))

(define (scan-files root-dir opts files root-name)
  (define top-n   (hash-ref opts 'top 8))
  (define verify? (hash-ref opts 'verify #f))

  (define host-code (make-hash))
  (define failed '())
  (for ([f (in-list files)])
    (define src (file->string f))
    (define uir (with-handlers ([exn:fail? (λ (e) #f)])
                  (python-source->uir src)))
    (if uir
        (hash-set! host-code (path->string f) uir)
        (set! failed (cons f failed))))

  ;; Import surface for the whole-program fixpoint.
  (define specs '())
  (define packages (set))
  (define comp-modpath (make-hash))
  (for ([f (in-list files)])
    (define src (file->string f))
    (define modpath
      (for/or ([r (in-list (list (cons root-name root-dir)))])
        (define rel (find-relative-path (cdr r) f))
        (and (not (string-prefix? (path->string rel) ".."))
             (path->module-path (car r) (cdr r) f))))
    (when modpath
      (hash-set! comp-modpath (path->string f) modpath)
      (when (regexp-match? #rx"__init__[.]py$" (path->string f))
        (set! packages (set-add packages modpath)))
      (define tks (for/list ([t (in-list (py-tokenize src))])
                    (cons (py-tok-type t) (py-tok-value t))))
      (set! specs (cons (module-spec modpath
                                     (or (hash-ref host-code (path->string f) #f)
                                         (uir-block '()))
                                     (extract-imports tks))
                        specs))))

  (define mods (build-module-table specs))
  (define caps (derive-class-method-effects mods))
  (define supers (derive-class-supers mods))
  (define env (import-env comp-modpath (resolve-exports mods packages) caps))

  (define tally (box '()))
  (define rows
    (infer-effects-all (for/list ([(c p) (in-hash host-code)]) (cons c p))
                       #:import-env env
                       #:unknown-tally tally))

  (define fns (hash-count rows))
  (define unknown-fns 0)
  (define unknown-files (set))
  (define effectful 0)
  (for ([(k row) (in-hash rows)])
    (define names (map effect-name-symbol (uir-effect-row-entries row)))
    (when (pair? names) (set! effectful (add1 effectful)))
    (when (memq 'unknown names)
      (set! unknown-fns (add1 unknown-fns))
      (set! unknown-files (set-add unknown-files (car k)))))

  (define hist (effect-histogram rows))
  (define db-count (let ([e (assq 'db hist)]) (if e (second e) 0)))
  (define collisions
    (apply append (for/list ([p (in-list (hash-values host-code))])
                    (orm-collision-sites p))))
  (define shape-counts (make-hash))
  (for ([rec (in-list (unbox tally))])
    (hash-update! shape-counts (third rec) add1 0))

  (printf "~a\tfiles ~a (parsed ~a, failed ~a)\tfns ~a (eff ~a, unk ~a/~a)\n"
          root-name (length files) (hash-count host-code) (length failed)
          fns effectful unknown-fns (set-count unknown-files))
  (printf "  effects: ~a\n" (hist-str hist))
  (printf "  orm-collide: ~a sites; top: ~a\n" (length collisions) (pairs-str (top-pairs collisions top-n)))
  (printf "  shapes: ~a\n" (shapes-str shape-counts))

  (when verify?
    (define spec-id (regexp-replace* #rx"[^A-Za-z0-9_]" root-name "_"))
    (define spec-text
      (string-append
       "system " spec-id " {\n"
       "  capability Cache { get(key: String) -> String }\n"
       "  given Cache\n"
       "  component " spec-id " needs Cache { lookup(key) = get(key) }\n"
       "}\n"))
    (define merged (merge-programs (hash-values host-code)))
    (define findings
      (with-handlers ([exn:fail? (λ (e) (printf "  verify: error — ~a\n" (exn-message e)) (flush-output) '())])
        (verify (load-effervesce spec-text)
                (hash spec-id merged)
                (hash "Cache" (list 'cache)))))
    (printf "  verify: ~a findings~a\n"
            (length findings)
            (if (null? findings) ""
                (string-append " — "
                               (string-join
                                (map (λ (f) (format "[~a:~a]" (finding-checker f) (finding-kind f)))
                                     (take findings (min 4 (length findings))))
                                " ")))))
  (flush-output))

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define opts (make-hash))
  (define dirs '())
  (let loop ([as args])
    (cond
      [(null? as) (void)]
      [(string=? (car as) "--max-files")
       (hash-set! opts 'max-files (string->number (second as)))
       (loop (cddr as))]
      [(string=? (car as) "--max-size")
       (hash-set! opts 'max-size (string->number (second as)))
       (loop (cddr as))]
      [(string=? (car as) "--top")
       (hash-set! opts 'top (string->number (second as)))
       (loop (cddr as))]
      [(string=? (car as) "--verify")
       (hash-set! opts 'verify #t)
       (loop (cdr as))]
      [(string-prefix? (car as) "--")
       (loop (cdr as))]
      [else
       (set! dirs (cons (car as) dirs))
       (loop (cdr as))]))
  (unless (pair? dirs)
    (eprintf "usage: review-sample.rkt <dir> [<dir> ...] [--max-files N] [--max-size KB] [--verify] [--top N]\n")
    (exit 1))
  (for ([dir (in-list (reverse dirs))])
    (scan-dir (simplify-path (path->complete-path dir) #f) opts)))

(main)

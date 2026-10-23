#lang racket

;; effervesce/tools/type-graph.rkt
;;
;; Whole-tree Django type-effect catalogue. Feeds every Python file through
;; the front end (parse -> lower), infers effect rows per named function,
;; projects the result into the typed-hypergraph vocabulary (racklr/hypergraph
;; + effervesce/check/graph), indexes it, and reports the aggregate type
;; effect algebra plus demonstration queries.
;;
;; Usage:
;;   racket tools/type-graph.rkt <dir> [--verbose] [--ast]
;;
;; --ast additionally materializes each file's UIR into the atom set (the
;; AtomSpace-style "materialize" projection), which is O(nodes) and can be
;; large; it is off by default.

(require racklr/uir
         racklr/effects/infer
         racklr/effects/contract
         racklr/hypergraph
         "../check/graph.rkt"
         "../check/host-python.rkt")

;; ── directory walk ───────────────────────────────────────────────────

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

;; ── UIR effect entry -> hypergraph effect-entry hyperedge ────────────

(define (entry->hg e)
  (effect-entry
   (effect-name-symbol e)
   (uir-effect-entry-modality e)
   (entry-ops e)
   (uir-effect-entry-budget e)
   (and (uir-effect-entry-scope e)
        (string->symbol (uir-symbol-name (uir-effect-entry-scope e))))))

(define (entry->string l)
  (define out (link-out l))
  (format "modality=~a ops=~a budget=~a scope=~a"
          (node-name (second out))
          (node-name (third out))
          (node-name (fourth out))
          (node-name (fifth out))))

;; ── index-based graph queries ────────────────────────────────────────

;; Component names whose fn-effects edge points at `eff`.
(define (components-with-effect idx eff)
  (remove-duplicates
   (for/list ([l (in-list (index-links idx 'fn-effects))]
              #:when (eq? (node-name (second (link-out l))) eff))
     (node-name (first (link-out l))))))

;; ── main ─────────────────────────────────────────────────────────────

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define dir (and (pair? args) (car args)))
  (define verbose (member "--verbose" args))
  (define with-ast? (member "--ast" args))
  (unless dir
    (eprintf "usage: type-graph.rkt <dir> [--verbose] [--ast]\n")
    (exit 1))

  (define files
    (sort (find-py-files dir)
          (λ (a b) (string<? (path->string a) (path->string b)))))
  (define host-code (make-hash))
  (define failed '())

  ;; Aggregate effect algebra: name -> (hash effect-entry-link -> fn-count),
  ;; and name -> (listof component) for the per-effect file provenance.
  (define by-name (make-hash))
  (define files-by-name (make-hash))
  (define entry-atoms '())
  (define comp-effects (make-hash))   ;; component -> (listof effect-name)
  (define file-rows (make-hash))      ;; component -> rows hash (cached)
  (define fn-count 0)
  (define effectful-fn-count 0)

  (for ([f (in-list files)])
    (define uir (python-source->uir (file->string f)))
    (cond
      [uir (hash-set! host-code (path->string f) uir)]
      [else (set! failed (cons f failed))]))

  ;; Whole-program inference once: a call through an inheritance chain that
  ;; crosses file boundaries (`self.save()` here, `Model.save` defined in
  ;; another file) becomes an edge instead of `unknown`. Rows are keyed
  ;; (component . name); each file's own rows are projected back out by
  ;; component so the per-file provenance breakdown is preserved.
  (define global-rows
    (infer-effects-all (for/list ([(c p) (in-hash host-code)]) (cons c p))))
  (for ([(comp program) (in-hash host-code)])
    (define rows (make-hash))
    (for ([(k row) (in-hash global-rows)]
          #:when (equal? (car k) comp))
      (hash-set! rows (cdr k) row))
    (hash-set! file-rows comp rows)
    (for ([row (in-hash-values rows)])
      (set! fn-count (add1 fn-count))
      (define entries (uir-effect-row-entries row))
      (when (pair? entries) (set! effectful-fn-count (add1 effectful-fn-count)))
      (for ([e (in-list entries)])
        (define hg (entry->hg e))
        (define name (effect-name-symbol e))
        (set! entry-atoms (cons hg entry-atoms))
        (define inner (hash-ref by-name name #f))
        (unless inner
          (set! inner (make-hash))
          (hash-set! by-name name inner))
        (hash-set! inner hg (add1 (hash-ref inner hg 0)))
        (define fl (hash-ref files-by-name name '()))
        (unless (member comp fl)
          (hash-set! files-by-name name (cons comp fl)))
        (hash-update! comp-effects comp
                      (λ (l) (if (member name l) l (cons name l)))
                      '()))))

  ;; Project into the hypergraph vocabulary (fn-effects / effect-touches
  ;; hyperedges + the effect-entry hyperedges + optional materialized AST).
  (define fn-effect-atoms
    (append-map (λ (c)
                  (map (λ (e) (fn-effects-link c e)) (hash-ref comp-effects c '())))
                (hash-keys comp-effects)))
  (define touch-atoms
    (append-map (λ (c)
                  (for/list ([e (in-list
                                 (append-map uir-effect-row-entries
                                             (hash-values (hash-ref file-rows c))))]
                             #:when (uir-effect-entry-scope e))
                    (effect-touches-link (effect-name-symbol e)
                                         (string->symbol
                                          (uir-symbol-name (uir-effect-entry-scope e))))))
                (hash-keys host-code)))
  (define atoms
    (remove-duplicates
     (append fn-effect-atoms
             touch-atoms
             (if with-ast?
                 (append-map (λ (c) (materialize (hash-ref host-code c)))
                             (hash-keys host-code))
                 '())
             entry-atoms)))
  (define idx (make-index atoms))

  ;; ── report ─────────────────────────────────────────────────────────

  (printf "=== Django type-effect catalogue ===\n")
  (printf "files scanned:  ~a\n" (length files))
  (printf "files parsed:   ~a\n" (hash-count host-code))
  (printf "files failed:   ~a\n" (length failed))
  (when (and verbose (pair? failed))
    (for ([f (in-list failed)]) (printf "  FAIL ~a\n" f)))
  (printf "named functions: ~a\n" fn-count)
  (printf "effectful functions: ~a\n" effectful-fn-count)

  (printf "\nEffect algebra (name -> distinct entries):\n")
  (for ([name (in-list (sort (hash-keys by-name) symbol<?))])
    (printf "  ~a  (functions-touching: ~a, files-touching: ~a)\n"
            name
            (for/sum ([c (in-hash-values (hash-ref by-name name))]) c)
            (length (hash-ref files-by-name name '())))
    (for ([(hg c) (in-hash (hash-ref by-name name))])
      (printf "      ~a  x~a\n" (entry->string hg) c)))

  (printf "\nType-graph (hypergraph projection):\n")
  (printf "  atoms: ~a\n" (length atoms))
  (printf "  node types:  ~a\n" (remove-duplicates (map node-type (filter node? atoms))))
  (printf "  link types:  ~a\n" (remove-duplicates (map link-type (filter link? atoms))))

  (printf "\nSearch demonstrations (hg-index / hg-query):\n")
  (for ([eff (in-list '(db cache network llm unknown))])
    (define comps (components-with-effect idx eff))
    (unless (null? comps)
      (printf "  ~a: ~a file(s)\n" eff (length comps))
      (when verbose
        (for ([c (in-list (sort comps string<?))]) (printf "      ~a\n" c)))))
  (define db (components-with-effect idx 'db))
  (define cache (components-with-effect idx 'cache))
  (define both (filter (λ (c) (member c cache)) db))
  (printf "  db+cache (both): ~a file(s)\n" (length both))

  ;; AtomSpace-style pattern query over the effect hyperedges.
  (define matches
    (hg-query (effect-entry (var 'e) (var 'm) (var 'o) (var 'b) (var 's)) atoms))
  (printf "  hg-query effect-entry pattern -> ~a matches\n" (length matches))
  (define modalities
    (remove-duplicates (map (λ (h) (hash-ref h 'm)) matches)))
  (printf "  modalities observed: ~a\n" modalities))

(main)

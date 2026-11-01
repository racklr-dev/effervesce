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
         racklr/effects/imports
         racklr/hypergraph
         "../check/graph.rkt"
         "../check/host-python.rkt"
         "graph-dump.rkt"
         "scoreboard.rkt")

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

;; Canonical dotted module name of a scanned root directory. Walk up past
;; every __init__.py-bearing ancestor (each is one package component) and join
;; the names, so scanning `django/django/core` yields "django.core" — matching
;; the import sources in the scanned files and the effect-tag config rules —
;; instead of the bare basename "core" (which matched nothing). A root with no
;; package ancestor falls back to its basename.
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

;; ── per-function effect rows (schema: fn-effect-row) ────────────────
;;
;; Schema addition (R5). The goal's `fn-effects` link is component(file)-level;
;; this adds the per-function half the coverage gate needs. Every named
;; function becomes a `function` node and carries one variadic hyperedge:
;;
;;   (fn-effect-row (function fqname) effect-entry...)
;;
;; where a pure function carries a single `pure` effect node in place of an
;; entry list. Invariant: distinct `function` nodes == named functions.
;; Query: "what does f touch?" = (index-links idx 'fn-effect-row) filtered on
;; the function node.
(define (fn-effect-row-atoms file-rows)
  (append-map
   (λ (c)
     (define rows (hash-ref file-rows c))
     (append-map
      (λ (nm)
        (define row (hash-ref rows nm))
        (define entries (map entry->hg (uir-effect-row-entries row)))
        (define fq (format "~a::~a" c nm))
        (if (null? entries)
            (list (link 'fn-effect-row (list (node 'function fq) (node 'effect 'pure))))
            (list (link 'fn-effect-row (cons (node 'function fq) entries)))))
      (sort (hash-keys rows) symbol<?)))
   (sort (hash-keys file-rows) string<?)))

;; ── main ─────────────────────────────────────────────────────────────

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define verbose (member "--verbose" args))
  (define with-ast? (member "--ast" args))
  (define dump? (member "--dump" args))
  (define dirs (filter (λ (a) (not (string-prefix? a "--"))) args))
  (unless (pair? dirs)
    (eprintf "usage: type-graph.rkt <dir> [<dir> ...] [--verbose] [--ast] [--dump]\n")
    (exit 1))

  ;; Each root is (name . dir); name is the canonical dotted package name used
  ;; to build dotted module paths (package-root-name walks up the __init__.py
  ;; chain so a subpackage root keeps its full import prefix).
  (define roots
    (for/list ([dir (in-list dirs)])
      (define root-dir (simplify-path (path->complete-path dir) #f))
      (cons (package-root-name root-dir) root-dir)))

  (define files
    (sort (apply append (map (λ (r) (find-py-files (cdr r))) roots))
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

  ;; Import-surface material for the effect fixpoint: per-file token imports +
  ;; module path, harvested into a module table, resolved to an export graph,
  ;; and derived into Class.method → effect-cap.
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

  ;; Whole-program inference once: a call through an inheritance chain that
  ;; crosses file boundaries (`self.save()` here, `Model.save` defined in
  ;; another file) becomes an edge instead of `unknown`; an imported-name
  ;; receiver resolves through the import graph to its capability. Rows are
  ;; keyed (component . name); each file's own rows are projected back out by
  ;; component so the per-file provenance breakdown is preserved.
  (define global-rows
    (infer-effects-all (for/list ([(c p) (in-hash host-code)]) (cons c p))
                       #:import-env env))
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
  ;; Per-function effect rows (schema: fn-effect-row) — the coverage gate.
  (define fn-row-atoms (fn-effect-row-atoms file-rows))
  ;; Capability-level graph recovered from the same inferred rows: components
  ;; with needs/provides, capability-effect interpretations, action-writes and
  ;; effect-touches. Without this the host projection only had the effect half
  ;; (fn-effects / fn-effect-row) and the census reported the rest MISSING.
  (define host-cap-atoms (host-capability-atoms host-code file-rows))
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
  ;; Typed capability atoms: each effect-entry's ops are now typed symbols
  ;; (`db<QuerySet>.read`), so decompose them into receiver-type nodes plus
  ;; `effect-op` hyperedges. Separately emit the MRO InheritanceLink surface
  ;; from the harvested super graph.
  (define typed-op-atoms
    (append-map
     (λ (entry)
       (define ops (node-name (third (link-out entry))))
       (for*/list ([op (in-list ops)]
                   [parts (in-value (typed-op-parts op))]
                   #:when parts)
         (effect-op (first parts) (second parts) (third parts))))
     entry-atoms))
  (define inherits-atoms
    (for*/list ([(child supers-list) (in-hash supers)]
                [parent (in-list supers-list)])
      (inherits child parent)))
  (define atoms
    (flatten-atoms
     (append fn-effect-atoms
             fn-row-atoms
             host-cap-atoms
             touch-atoms
             (if with-ast?
                 (append-map (λ (c) (materialize (hash-ref host-code c)))
                             (hash-keys host-code))
                 '())
             entry-atoms
             typed-op-atoms
             inherits-atoms)))
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

  ;; R4: --verbose prints one `file::name -> effect-row` line per function,
  ;; so a reviewer can read which functions carry which effects without a
  ;; separate query.
  (when verbose
    (printf "\nPer-function effect rows (file::name -> effects):\n")
    (for ([c (in-list (sort (hash-keys file-rows) string<?))])
      (for ([nm (in-list (sort (hash-keys (hash-ref file-rows c)) symbol<?))])
        (define row (hash-ref (hash-ref file-rows c) nm))
        (define names (remove-duplicates
                       (map effect-name-symbol (uir-effect-row-entries row))))
        (printf "  ~a::~a -> ~a\n" c nm
                (if (null? names)
                    "pure"
                    (string-join (map symbol->string names) ","))))))

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
  (printf "  modalities observed: ~a\n" modalities)

  ;; ── OpenCog-compatible demonstration ────────────────────────────────
  ;;
  ;; The typed capability (`db<QuerySet>.read`) and MRO surface projected as
  ;; Atomese S-expressions: distinct receiver nodes chained by InheritanceLink,
  ;; each op an EvaluationLink whose predicate names domain.verb.
  (define op-atoms (filter (λ (a) (and (link? a) (eq? (link-type a) 'effect-op))) atoms))
  (define inh-atoms (filter (λ (a) (and (link? a) (eq? (link-type a) 'inherits))) atoms))
  (printf "\nAtomese (OpenCog-compatible) projection:\n")
  (printf "  effect-op hyperedges:  ~a\n" (length op-atoms))
  (printf "  inherits hyperedges:   ~a\n" (length inh-atoms))
  (define (show-prefix label xs)
    (unless (null? xs)
      (printf "  ~a\n" label)
      (for ([a (in-list (take xs (min 6 (length xs))))])
        (printf "    ~a\n" (atom->atomese a)))))
  (show-prefix "typed operations (sample):" op-atoms)
  (show-prefix "inheritance (sample):" inh-atoms)

  (report-scoreboard fn-count effectful-fn-count
                     entry-atoms by-name files-by-name
                     fn-effect-atoms fn-row-atoms atoms)

  (when dump?
    (write-dump "effervesce-out" atoms idx file-rows)))

(main)

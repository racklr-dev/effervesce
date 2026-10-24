#lang racket

;; effervesce/tools/graph-dump.rkt — serialization of a materialized graph.
;;
;; Owns the --dump projection: the flat atomspace closure as JSON (a faithful,
;; self-contained node/link set) plus an OpenCog Atomese S-expression per line,
;; and a compact component->effects table. Moved verbatim out of type-graph.rkt
;; so that tool stays under the 400-line layout rule; no behaviour change.

(require json
         racklr/uir
         racklr/effects/contract
         racklr/hypergraph)

(provide write-dump)

;; Coerce atom payloads (symbols, op lists, #f absent-slots) into legal JSON
;; values: symbols -> strings, lists -> recursively, #f -> null.
(define (json-scalar x)
  (cond
    [(symbol? x) (symbol->string x)]
    [(list? x) (map json-scalar x)]
    [(eq? x #f) 'null]
    [else x]))

;; An atom as a JSON-ready structure: nodes keep their (type, name); links
;; carry their ordered out-list recursively, so the JSON is a faithful,
;; self-contained serialization of the flat atomspace closure.
(define (atom->jsexpr a)
  (cond
    [(node? a) (hasheq 'kind "node" 'type (json-scalar (node-type a))
                       'name (json-scalar (node-name a)))]
    [(link? a) (hasheq 'kind "link" 'type (json-scalar (link-type a))
                       'out (map atom->jsexpr (link-out a)))]
    [(var? a) (hasheq 'kind "var" 'name (json-scalar (var-name a)))]
    [else (hasheq 'kind "scalar" 'value (json-scalar a))]))

;; Link types worth bucketing in the report (the queryable vocabulary).
(define dump-link-types
  '(fn-effects effect-touches effect-op inherits effect capability-effect
    component-needs component-provides capability-interprets action-writes))

(define (write-dump outdir atoms idx rows)
  (make-directory* outdir)
  (define json-out (build-path outdir "effect-graph.json"))
  (define atomese-out (build-path outdir "effect-graph.scm"))

  ;; Full closure so every nested node is a first-class atom (as an AtomSpace
  ;; holds it), ordered deterministically for stable output.
  (define all (flatten-atoms atoms))
  (define sorted (sort all (λ (a b) (string<? (format "~a" a) (format "~a" b)))))

  ;; JSON: the graph (node/link atom set) plus a compact component->effects
  ;; table so "what does this handler touch" has an O(1) JSON answer.
  (define comp-effects (make-hash))
  (for* ([(c r) (in-hash rows)]
         [row (in-list (hash-values r))]
         [e (in-list (uir-effect-row-entries row))]
         [n (in-value (effect-name-symbol e))]
         #:when (not (eq? n 'pure)))
    (hash-update! comp-effects c (λ (l) (if (member n l) l (cons n l))) '()))
  (call-with-output-file json-out #:exists 'replace
    (λ (out)
      (write-json
       (hasheq
        'atoms (map atom->jsexpr sorted)
        'component-effects
        (for/list ([(c es) (in-hash comp-effects)])
          (hasheq 'component c
                  'effects (map json-scalar (sort es symbol<?))))
        'effect-counts
        (for/list ([(n h) (in-hash (atom-effect-counts all))])
          (hasheq 'effect (symbol->string n) 'entries h)))
       out)))

  ;; Atomese: every atom on its own line, deduplicated by the closure.
  (call-with-output-file atomese-out #:exists 'replace
    (λ (out)
      (for ([a (in-list sorted)])
        (fprintf out "~a\n" (atom->atomese a)))))

  (printf "\nMaterialized dump written:\n")
  (printf "  ~a  (~a atoms)\n" json-out (length sorted))
  (printf "  ~a  (~a atoms)\n" atomese-out (length sorted))
  (printf "  components with effects: ~a\n" (hash-count comp-effects))
  (printf "  link-type census:\n")
  (for* ([t (in-list dump-link-types)]
         [n (in-value (length (filter (λ (a) (and (link? a) (eq? (link-type a) t)))
                                      sorted)))]
         #:when (positive? n))
    (printf "    ~a: ~a\n" t n)))

;; Effect name -> count of distinct entry hyperedges, over the closure.
(define (atom-effect-counts atoms)
  (define h (make-hash))
  (for ([a (in-list atoms)]
        #:when (and (link? a) (eq? (link-type a) 'effect)))
    (hash-update! h (effect-key a) add1 0))
  h)

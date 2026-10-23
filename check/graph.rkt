#lang racket

;; effervesce/check/graph.rkt
;;
;; Project an Effervesce uir-eff-system (plus optional host UIR and the
;; bridge interpretation table) into the typed-hypergraph vocabulary from
;; racklr/hypergraph.rkt.
;;
;; The graph is a DERIVED, queryable view. The checkers keep working on
;; structs; this exists for fast query and to give every front end (spec,
;; source, binary) one common shape.
;;
;; Node types: component, capability, effect, state, action, resource.
;; Link types:
;;   component-needs       (component, capability)    `component C needs R`
;;   component-provides    (component, capability)    `component C provides P`
;;   capability-interprets (capability, capability)   `E via R`
;;   capability-effect     (capability, effect)       bridge interpretation table
;;   given                 (capability)               terminal, environment-provided
;;   action-writes         (action, state)            `state := expr` step
;;   fn-effects            (component, effect)        host-code inferred effects
;;   effect-touches        (effect, resource)         effect entry carrying a scope
;;
;; Capability identity reuses the type checker's `ref-key` (name + type
;; args) so the graph and the checkers never disagree about what a
;; capability reference is.

(require racklr/uir
         racklr/hypergraph
         racklr/effects/infer
         racklr/effects/contract
         "util.rkt")

(provide
  ;; projection
  system-graph
  system-graph+host
  ;; index
  hg-index hg-index?
  make-index
  index-nodes index-node index-links
  ;; node constructors
  component-node capability-node effect-node state-node action-node resource-node
  ;; link constructors
  component-needs-link component-provides-link
  capability-interprets-link capability-effect-link given-link
  action-writes-link fn-effects-link effect-touches-link)

;; ── atoms ───────────────────────────────────────────────────────────

(define (component-node name) (node 'component name))

;; A capability's identity is the checker's ref-key: (cons name type-args).
(define (capability-node ref) (node 'capability (ref-key ref)))

(define (effect-node name) (node 'effect name))
(define (state-node name) (node 'state name))
(define (action-node name) (node 'action name))
(define (resource-node scope) (node 'resource scope))

;; ── links ───────────────────────────────────────────────────────────

(define (component-needs-link cname ref)
  (link 'component-needs (list (component-node cname) (capability-node ref))))

(define (component-provides-link cname ref)
  (link 'component-provides (list (component-node cname) (capability-node ref))))

(define (capability-interprets-link effect ref)
  (link 'capability-interprets (list (capability-node effect) (capability-node ref))))

(define (capability-effect-link cap-name eff-name)
  (link 'capability-effect (list (node 'capability (cons cap-name '()))
                                 (effect-node eff-name))))

(define (given-link ref)
  (link 'given (list (capability-node ref))))

(define (action-writes-link aname sname)
  (link 'action-writes (list (action-node aname) (state-node sname))))

(define (fn-effects-link cname eff-name)
  (link 'fn-effects (list (component-node cname) (effect-node eff-name))))

(define (effect-touches-link eff-name scope)
  (link 'effect-touches (list (effect-node eff-name) (resource-node scope))))

;; ── projection ──────────────────────────────────────────────────────

;; State names written by an action's `x := expr` steps.
(define (action-written-states action)
  (for/list ([s (in-list (uir-eff-action-steps action))]
             #:when (uir-set!? s))
    (uir-symbol-name (uir-set!-name s))))

;; A bare capability ref (no type args) for a capability declaration.
(define (decl-cap-ref decl)
  (uir-eff-ref (uir-eff-capability-name decl) '()))

(define (decl->atoms d)
  (cond
    [(uir-eff-capability? d)
     (list (capability-node (decl-cap-ref d)))]
    [(uir-eff-given? d)
     (map given-link (uir-eff-given-row d))]
    [(uir-eff-handler? d)
     (define e (uir-eff-handler-effect d))
     (define row (uir-eff-handler-row d))
     (append (list (capability-node e))
             (map capability-node row)
             (map (λ (r) (capability-interprets-link e r)) row))]
    [(uir-eff-component? d)
     (define cname (uir-symbol-name (uir-eff-component-name d)))
     (define refs (append (uir-eff-component-needs d)
                          (uir-eff-component-provides d)))
     (append (list (component-node cname))
             (map (λ (r) (component-needs-link cname r)) (uir-eff-component-needs d))
             (map (λ (r) (component-provides-link cname r)) (uir-eff-component-provides d))
             (map capability-node refs))]
    [(uir-eff-state? d)
     (list (state-node (uir-symbol-name (uir-eff-state-name d))))]
    [(uir-eff-action? d)
     (define aname (uir-symbol-name (uir-eff-action-name d)))
     (cons (action-node aname)
           (map (λ (s) (action-writes-link aname s)) (action-written-states d)))]
    [else '()]))

(define (system-graph sys)
  (remove-duplicates
   (append-map decl->atoms (uir-eff-system-decls sys))))

;; ── host projection ─────────────────────────────────────────────────

(define (program-effect-entries program)
  (append-map uir-effect-row-entries (hash-values (infer-effects program))))

;; Distinct effect names inferred for one host program.
(define (program-effect-names program)
  (remove-duplicates
   (map effect-name-symbol (program-effect-entries program))))

(define (host-atoms host-code interpretations)
  (append
   ;; fn-effects: component -> each inferred host effect
   (append-map
    (λ (cname)
      (define program (hash-ref host-code cname #f))
      (if program
          (map (λ (e) (fn-effects-link cname e)) (program-effect-names program))
          '()))
    (hash-keys host-code))
   ;; capability-effect: interpretation table edges
   (append-map
    (λ (cap-name)
      (map (λ (e) (capability-effect-link cap-name e))
           (hash-ref interpretations cap-name '())))
    (hash-keys interpretations))
   ;; effect-touches: inferred entries that carry a scope (populated once
   ;; annotations/BAP feed scopes through inference; empty today)
   (append-map
    (λ (cname)
      (define program (hash-ref host-code cname #f))
      (if program
          (for/list ([e (in-list (program-effect-entries program))]
                     #:when (uir-effect-entry-scope e))
            (effect-touches-link (effect-name-symbol e)
                                 (uir-symbol-name (uir-effect-entry-scope e))))
          '()))
    (hash-keys host-code))))

;; Host program ASTs, materialized into the queryable graph so any UIR node
;; (or its S-expression form) can be found and traversed.
(define (host-ast-atoms host-code)
  (append-map (λ (cname)
                (define program (hash-ref host-code cname #f))
                (if program (materialize program) '()))
              (hash-keys host-code)))

(define (system-graph+host sys host-code interpretations)
  (remove-duplicates
   (append (system-graph sys)
           (host-atoms host-code interpretations)
           (host-ast-atoms host-code))))

;; ── inverted index ──────────────────────────────────────────────────

;; O(1) node lookup by (type, name); O(1) link lookup by type.
(struct hg-index (by-node by-link) #:transparent)

(define (make-index atoms)
  (define by-node (make-hash))
  (define by-link (make-hash))
  (for ([a (in-list atoms)])
    (cond
      [(node? a)
       (hash-update! by-node (node-type a)
                     (λ (h) (hash-set h (node-name a) a))
                     (hash))]
      [(link? a)
       (hash-update! by-link (link-type a)
                     (λ (l) (cons a l))
                     '())]))
  (hg-index by-node by-link))

(define (index-nodes idx type)
  (hash-values (hash-ref (hg-index-by-node idx) type (hash))))

(define (index-node idx type name)
  (hash-ref (hash-ref (hg-index-by-node idx) type (hash)) name #f))

(define (index-links idx type)
  (hash-ref (hg-index-by-link idx) type '()))

(module+ main
  (displayln "effervesce/check/graph — system graph loaded."))

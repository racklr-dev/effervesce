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
;; Node types: component, capability, op, handler, effect, state, action,
;; resource, policy, invariant.
;; Link types:
;;   component-needs       (component, capability)    `component C needs R`
;;   component-provides    (component, capability)    `component C provides P`
;;   capability-interprets (capability, capability)   `E via R`
;;   capability-effect     (capability, effect)       bridge interpretation table
;;   capability-op         (capability, op)           an op the capability declares
;;   component-op          (component, op)            an op the component implements
;;   component-call        (component, op)            an op the component body calls
;;   handler-interprets    (handler, capability)      a handler's `via` row
;;   handler-op            (handler, op)              an op the handler implements
;;   handler-call          (handler, op)              an op the handler body calls
;;   given                 (capability)               terminal, environment-provided
;;   action-writes         (action, state)            `state := expr` step
;;   fn-effects            (component, effect)        host-code inferred effects
;;   effect-touches        (effect, resource)         effect entry carrying a scope
;;   effect-site           (component, effect, site)  classifying call-site provenance
;;
;; Capability identity reuses the type checker's `ref-key` (name + type
;; args) so the graph and the checkers never disagree about what a
;; capability reference is.
;;
;; The same file is the contextual-scoping kernel: every link type above is
;; declared in `edge-shapes` with the slots it joins, and a query walks
;; declared `hop`s from an anchor node (see "contextual scoping" below).
;; Undeclared edges cannot be traversed, so a query cannot silently walk an
;; edge the vocabulary does not define.

(require racklr/uir
         racklr/hypergraph
         racklr/effects/infer
         racklr/effects/contract
         "host-graph.rkt"
         "util.rkt")

(provide
  ;; projection
  system-graph
  system-graph+host
  ;; capability-level recovery from inferred host rows
  host-capability-atoms
  ;; index
  hg-index hg-index? hg-index-by-node hg-index-by-link
  make-index
  index-nodes index-node index-links index-link-types
  ;; node constructors
  component-node capability-node op-node handler-node effect-node state-node
  state-decl-node action-node resource-node policy-node invariant-node site-node
  ;; link constructors
  component-needs-link component-provides-link
  capability-interprets-link capability-effect-link given-link
  capability-op-link component-op-link component-call-link
  handler-effect-link handler-interprets-link handler-op-link handler-call-link
  action-writes-link fn-effects-link effect-touches-link effect-site-link
  declares-state-link invariant-references-link
  ;; capability-reference identity
  ref-key->ref)

;; ── atoms ───────────────────────────────────────────────────────────

(define (component-node name) (node 'component name))

;; A capability's identity is the checker's ref-key: (cons name type-args).
(define (capability-node ref) (node 'capability (ref-key ref)))

;; An operation name (the checker keys ops by their bare name).
(define (op-node op-name) (node 'op op-name))

;; A handler is a declaration, not a capability: its identity is the
;; declaration index plus the ref-key it interprets, so two handlers for the
;; same capability stay distinct (the checker reports each of them).
(define (handler-node i ref) (node 'handler (list i (ref-key ref))))

(define (effect-node name) (node 'effect name))
(define (state-node name) (node 'state name))

;; A state declaration is declaration-distinct (duplicate declarations are a
;; model finding), so its identity is the declaration index plus the name.
(define (state-decl-node i name) (node 'state-decl (list i name)))
(define (action-node name) (node 'action name))
(define (resource-node scope) (node 'resource scope))

;; A policy declaration keeps its kind and predicate so a query can evaluate
;; the predicate against the graph and a finding can quote the source rule.
(define (policy-node i kind pred) (node 'policy (list i kind pred)))

;; An invariant declaration; the model questions anchor at these.
(define (invariant-node i) (node 'invariant i))

;; A classifying call site `function: callee`.
(define (site-node text) (node 'site text))

;; A capability reference back from the checker's ref-key (name + type args).
(define (ref-key->ref k)
  (uir-eff-ref (uir-symbol (car k)) (map uir-symbol (cdr k))))

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

(define (capability-op-link ref op-name)
  (link 'capability-op (list (capability-node ref) (op-node op-name))))

(define (component-op-link cname op-name)
  (link 'component-op (list (component-node cname) (op-node op-name))))

(define (component-call-link cname op-name)
  (link 'component-call (list (component-node cname) (op-node op-name))))

(define (handler-effect-link i ref)
  (link 'handler-effect (list (handler-node i ref) (capability-node ref))))

(define (handler-interprets-link i capref ref)
  (link 'handler-interprets (list (handler-node i capref) (capability-node ref))))

(define (handler-op-link i capref op-name)
  (link 'handler-op (list (handler-node i capref) (op-node op-name))))

(define (handler-call-link i capref op-name)
  (link 'handler-call (list (handler-node i capref) (op-node op-name))))

(define (given-link ref)
  (link 'given (list (capability-node ref))))

(define (action-writes-link aname sname)
  (link 'action-writes (list (action-node aname) (state-node sname))))

(define (fn-effects-link cname eff-name)
  (link 'fn-effects (list (component-node cname) (effect-node eff-name))))

(define (effect-touches-link eff-name scope)
  (link 'effect-touches (list (effect-node eff-name) (resource-node scope))))

(define (effect-site-link cname eff-name site)
  (link 'effect-site (list (component-node cname) (effect-node eff-name)
                           (site-node site))))

;; Declaration markers: a declared state and the free state names an
;; invariant reads. The structural model questions are traversals over these.
(define (declares-state-link i sname)
  (link 'declares-state (list (state-decl-node i sname) (state-node sname))))

(define (invariant-references-link i j v)
  (link 'invariant-references (list (invariant-node i) (node 'ref (list v j)))))

;; ── projection ──────────────────────────────────────────────────────

;; State names written by an action's `x := expr` steps.
(define (action-written-states action)
  (for/list ([s (in-list (uir-eff-action-steps action))]
             #:when (uir-set!? s))
    (uir-symbol-name (uir-set!-name s))))

;; A bare capability ref (no type args) for a capability declaration.
(define (decl-cap-ref decl)
  (uir-eff-ref (uir-eff-capability-name decl) '()))

;; Every function/operation name an expression invokes (mirrors the type
;; checker's `called-names`; the graph records invocations as edges, so the
;; invocation questions become traversals).
(define (called-names e)
  (cond
    [(uir-call? e)
     (define callee (uir-call-callee e))
     (append (if (uir-symbol? callee) (list (uir-symbol-name callee)) '())
             (append-map called-names (uir-call-args e)))]
    [(uir-block? e) (append-map called-names (uir-block-stmts e))]
    [(uir-paren? e) (called-names (uir-paren-inner e))]
    [(uir-get? e) (called-names (uir-get-base e))]
    [(uir-set!? e) (called-names (uir-set!-value e))]
    [else '()]))

;; Free state names an expression reads (var refs and field-access bases);
;; projected so the model's "undeclared state reference" question is a walk.
(define (collect-free-vars e)
  (cond
    [(uir-var? e) (list (uir-symbol-name (uir-var-name e)))]
    [(uir-get? e) (collect-free-vars (uir-get-base e))]
    [(uir-paren? e) (collect-free-vars (uir-paren-inner e))]
    [(uir-call? e) (append-map collect-free-vars (uir-call-args e))]
    [(uir-block? e) (append-map collect-free-vars (uir-block-stmts e))]
    [(uir-set!? e) (collect-free-vars (uir-set!-value e))]
    [else '()]))

;; `component`/`handler` operation clauses are (name params result-type body).
(define (op-quad-name op) (uir-symbol-name (first op)))
(define (op-quad-body op) (fourth op))

(define (decl->atoms d i)
  (cond
    [(uir-eff-capability? d)
     (define ref (decl-cap-ref d))
     (append (list (capability-node ref))
             (map (λ (op) (capability-op-link ref (uir-symbol-name
                                                   (uir-eff-capability-op-name op))))
                  (uir-eff-capability-ops d)))]
    [(uir-eff-given? d)
     (map given-link (uir-eff-given-row d))]
    [(uir-eff-handler? d)
     (define e (uir-eff-handler-effect d))
     (define row (uir-eff-handler-row d))
     (append (list (handler-node i e) (capability-node e))
             (map capability-node row)
             (list (handler-effect-link i e))
             (map (λ (r) (handler-interprets-link i e r)) row)
             (append-map
              (λ (op)
                (append
                 (list (handler-op-link i e (op-quad-name op)))
                 (for/list ([call (in-list (called-names (op-quad-body op)))])
                   (handler-call-link i e call))))
              (uir-eff-handler-ops d)))]
    [(uir-eff-component? d)
     (define cname (uir-symbol-name (uir-eff-component-name d)))
     (define refs (append (uir-eff-component-needs d)
                          (uir-eff-component-provides d)))
     (append (list (component-node cname))
             (map (λ (r) (component-needs-link cname r)) (uir-eff-component-needs d))
             (map (λ (r) (component-provides-link cname r)) (uir-eff-component-provides d))
             (map capability-node refs)
             (append-map
              (λ (op)
                (append
                 (list (component-op-link cname (op-quad-name op)))
                 (for/list ([call (in-list (called-names (op-quad-body op)))])
                   (component-call-link cname call))))
              (uir-eff-component-ops d)))]
    [(uir-eff-state? d)
     (define sname (uir-symbol-name (uir-eff-state-name d)))
     (list (state-node sname) (declares-state-link i sname))]
    [(uir-eff-action? d)
     (define aname (uir-symbol-name (uir-eff-action-name d)))
     (cons (action-node aname)
           (map (λ (s) (action-writes-link aname s)) (action-written-states d)))]
    [(uir-eff-policy? d)
     (list (policy-node i (uir-eff-policy-kind d) (uir-eff-policy-predicate d)))]
    [(uir-eff-invariant? d)
     (define expr (uir-eff-invariant-expr d))
     (cons (invariant-node i)
           (for/list ([v (in-list (collect-free-vars expr))]
                      [j (in-naturals)])
             (invariant-references-link i j v)))]
    [else '()]))

;; The type checker's `via-edges` is a hash from handler effect to row, so a
;; capability with several `via` handlers keeps only the last one. Project the
;; same edge set (the handler nodes themselves stay declaration-distinct for
;; the handler-completeness question).
(define (last-handler-interpretations decls)
  (define last (make-hash))
  (for ([d (in-list decls)] #:when (uir-eff-handler? d))
    (hash-set! last (ref-key (uir-eff-handler-effect d))
               (uir-eff-handler-row d)))
  (append-map (λ (k)
                (map (λ (r) (capability-interprets-link (ref-key->ref k) r))
                     (hash-ref last k)))
              (hash-keys last)))

(define (system-graph sys)
  (define decls (uir-eff-system-decls sys))
  (remove-duplicates
   (append (append-map (λ (d i) (decl->atoms d i)) decls (range (length decls)))
           (last-handler-interpretations decls))))

;; ── host projection ─────────────────────────────────────────────────

;; One inference pass per component: effect entries plus the provenance
;; table (name → effect → call sites) that the bridge question traverses.
(define (analyze-host-program program)
  (define prov (box (make-hash)))
  (define rows (infer-effects program #:provenance-table prov))
  (values (append-map uir-effect-row-entries (hash-values rows))
          (unbox prov)
          rows))

;; Capability-level atoms recovered from host rows, for components a declared
;; system does not declare: declared capability graphs stay authoritative.
(define (recovered-capability-atoms host-code analyses declared)
  (define keep? (λ (cname) (not (member cname declared))))
  (host-capability-atoms
   (for/hash ([(cname program) (in-hash host-code)] #:when (keep? cname))
     (values cname program))
   (for/hash ([a (in-list analyses)] #:when (keep? (first a)))
     (values (first a) (fifth a)))))

(define (host-atoms host-code interpretations [declared-components '()])
  (define analyses
    (for/list ([cname (in-list (hash-keys host-code))])
      (define-values (entries prov rows) (analyze-host-program (hash-ref host-code cname)))
      (list cname entries
            (remove-duplicates (map effect-name-symbol entries))
            prov
            rows)))
  (append
   ;; fn-effects: component -> each inferred host effect
   (append-map (λ (a) (map (λ (e) (fn-effects-link (first a) e)) (third a)))
               analyses)
   ;; capability-effect: interpretation table edges
   (append-map
    (λ (cap-name)
      (map (λ (e) (capability-effect-link cap-name e))
           (hash-ref interpretations cap-name '())))
    (hash-keys interpretations))
   ;; effect-touches: inferred entries that carry a scope (populated once
   ;; annotations/BAP feed scopes through inference; empty today)
   (append-map (λ (a)
                 (for/list ([e (in-list (second a))]
                            #:when (uir-effect-entry-scope e))
                   (effect-touches-link (effect-name-symbol e)
                                        (uir-symbol-name (uir-effect-entry-scope e)))))
               analyses)
   ;; effect-site: the exact classifying call sites behind each inferred
   ;; effect, rendered the way the bridge report shows them.
   (append-map (λ (a)
                 (for*/list ([(nm h) (in-hash (fourth a))]
                             [eff (in-hash-keys h)]
                             [site (in-list (hash-ref h eff '()))])
                   (effect-site-link (first a) eff (format "~a: ~a" nm site))))
               analyses)
   ;; the capability-level links recovered from the same inferred rows
   (recovered-capability-atoms host-code analyses declared-components)))

;; Host program ASTs, materialized into the queryable graph so any UIR node
;; (or its S-expression form) can be found and traversed.
(define (host-ast-atoms host-code)
  (append-map (λ (cname)
                (define program (hash-ref host-code cname #f))
                (if program (materialize program) '()))
              (hash-keys host-code)))

(define (declared-component-names sys)
  (for/list ([d (in-list (uir-eff-system-decls sys))]
             #:when (uir-eff-component? d))
    (uir-symbol-name (uir-eff-component-name d))))

(define (system-graph+host sys host-code interpretations)
  (remove-duplicates
   (append (system-graph sys)
           (host-atoms host-code interpretations (declared-component-names sys))
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

;; Declared link types present in the index, in stable order.
(define (index-link-types idx)
  (sort (hash-keys (hg-index-by-link idx)) symbol<?))

(module+ main
  (displayln "effervesce/check/graph — system graph loaded."))

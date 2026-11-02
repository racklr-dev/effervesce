#lang racket

;; effervesce/check/verify.rkt
;;
;; System 2: the exact path. Verification is a set of typed traversal queries
;; over the merged substrate (check/graph.rkt + check/scope.rkt): each query
;; anchors at a node kind (component, handler, capability, invariant, ...),
;; scopes along declared edge kinds, and turns the scoped subgraph into the
;; same normalized `finding` list the four checker modules report.
;;
;; The checkers keep their struct-level APIs for `check()`; `verify` is the
;; query-shaped path over the graph. A finding is (checker severity kind
;; message):
;;   checker  ∈ 'type | 'policy | 'model | 'bridge
;;   severity ∈ 'hard | 'note
;;   kind     a checker-specific symbol
;;
;; `verify-ok?` is true when there are no 'hard findings. Notes (e.g. an
;; invariant the model checker could not discharge) do not fail verification.
;;
;; One question stays a solver rather than a walk: whether an `always`
;; invariant holds in every reachable state. That explores the bounded
;; reachable-state space of `:=` transitions and evaluates the invariant
;; expression in each state — a search over states the graph does not
;; materialize — so its query keeps the exact solver as its predicate.
;; Everything else — handling, invocations, handler and provider
;; completeness, policies, the structural model findings, bridge coverage and
;; provenance — is a traversal over projected edges.

(require racklr/uir
         racklr/hypergraph
         "findings.rkt"
         "graph.rkt"
         "scope.rkt"
         "query.rkt"
         "util.rkt"
         "type.rkt"
         "policy.rkt"
         "model.rkt"
         "bridge.rkt")

(provide
  verify verify-ok? verify-report
  verify-checker-findings
  finding finding? finding-checker finding-severity finding-kind finding-message)


;; ── graph helpers used by the verification queries ──────────────────

(define (decls sys) (uir-eff-system-decls sys))
(define (components sys) (filter uir-eff-component? (decls sys)))
(define (handlers sys) (filter uir-eff-handler? (decls sys)))

(define (component-name c) (uir-symbol-name (uir-eff-component-name c)))

(define (component-roots sys)
  (map (λ (c) (component-node (component-name c))) (components sys)))

(define (handler-roots sys)
  (for/list ([d (in-list (decls sys))] [i (in-naturals)]
             #:when (uir-eff-handler? d))
    (handler-node i (uir-eff-handler-effect d))))

;; The capability a handler interprets, read off the projected handler-effect
;; edge (a handler is declaration-distinct, so its name alone is not the
;; capability name).
(define (handler-cap-name idx h)
  (car (node-name (first (reached idx (list h) 'handler-effect)))))

(define (policy-roots sys)
  (for/list ([d (in-list (decls sys))] [i (in-naturals)]
             #:when (uir-eff-policy? d))
    (policy-node i (uir-eff-policy-kind d) (uir-eff-policy-predicate d))))

(define (invariant-roots sys)
  (for/list ([d (in-list (decls sys))] [i (in-naturals)]
             #:when (uir-eff-invariant? d))
    (invariant-node i)))

(define (state-decl-roots sys)
  (for/list ([d (in-list (decls sys))] [i (in-naturals)]
             #:when (uir-eff-state? d))
    (state-decl-node i (uir-symbol-name (uir-eff-state-name d)))))

(define (action-roots sys)
  (for/list ([d (in-list (decls sys))]
             #:when (uir-eff-action? d))
    (action-node (uir-symbol-name (uir-eff-action-name d)))))

;; Nodes reached from `roots` over one declared edge kind.
(define (reached idx roots type)
  (remove-duplicates (follow idx roots (list (forward-hop type)))))

;; Operation names a node set owns over one declared edge kind.
(define (op-names-of-node-roots idx roots kind)
  (remove-duplicates
   (for/list ([n (in-list (reached idx roots kind))]) (node-name n))))

;; The capability that owns an operation: the last capability declaration
;; that declares it wins, matching the checker's op→owner table (the index
;; keeps newest links first).
(define (op-owner idx op)
  (for/first ([l (in-list (index-links idx 'capability-op))]
              #:when (equal? (node-name (second (link-out l))) op))
    (car (node-name (first (link-out l))))))

;; Operation names a capability name declares.
(define (declared-ops idx cap-name)
  (remove-duplicates
   (for/list ([l (in-list (index-links idx 'capability-op))]
              #:when (equal? (car (node-name (first (link-out l)))) cap-name))
     (node-name (second (link-out l))))))

;; ── type questions (handling + invocations) ─────────────────────────

;; Every `component needs R` whose capability is not handled: a scan of
;; component-needs edges, each answer decided by the handling-graph query.
(define (unhandled-needs-query sys)
  (query 'verify-unhandled-needs
         (λ (idx args) (component-roots sys))
         (single-step (forward-hop 'component-needs))
         (λ (idx args roots scopes)
           (append-map
            (λ (c scope)
              (for/list ([cap (in-list scope)]
                         #:when (eq? (node-type cap) 'capability)
                         #:unless (reachable? idx (node-name cap)))
                (type-finding
                 (unhandled-need (node-name c) (ref-key->ref (node-name cap))))))
            roots scopes))
         #f))

;; Every op a component invokes outside the capabilities its needs covers.
(define (unhandled-invocations-query sys)
  (query 'verify-unhandled-invocations
         (λ (idx args) (component-roots sys))
         (single-step (forward-hop 'component-call))
         (λ (idx args roots scopes)
           (append-map
            (λ (c scope)
              (define needs (map car (op-names-of-node-roots
                                      idx (list c) 'component-needs)))
              (for*/list ([op (in-list scope)]
                          #:when (eq? (node-type op) 'op)
                          [owner (in-value (op-owner idx (node-name op)))]
                          #:when owner
                          #:unless (member owner needs))
                (type-finding
                 (unhandled-invocation (node-name c) owner (node-name op)))))
            roots scopes))
         #f))

;; Every op a `via` handler's capability declares that the handler omits.
(define (missing-handler-ops-query sys)
  (query 'verify-missing-handler-ops
         (λ (idx args) (handler-roots sys))
         (single-step (forward-hop 'handler-op))
         (λ (idx args roots scopes)
           (append-map
            (λ (h scope)
              (define cap (handler-cap-name idx h))
              (for/list ([op (in-list (declared-ops idx cap))]
                         #:unless
                         (for/or ([impl (in-list scope)]
                                  #:when (eq? (node-type impl) 'op))
                           (equal? (node-name impl) op)))
                (type-finding (missing-handler-op cap op))))
            roots scopes))
         #f))

;; Every op a component's provided capability declares that it omits.
(define (missing-provided-ops-query sys)
  (query 'verify-missing-provided-ops
         (λ (idx args) (component-roots sys))
         (single-step (forward-hop 'component-provides))
         (λ (idx args roots scopes)
           (append-map
            (λ (c scope)
              (define implemented (reached idx (list c) 'component-op))
              (append-map
               (λ (cap)
                 (define cap-name (car (node-name cap)))
                 (for/list ([op (in-list (declared-ops idx cap-name))]
                            #:unless
                            (for/or ([impl (in-list implemented)])
                              (equal? (node-name impl) op)))
                   (type-finding (missing-provided-op (node-name c) cap-name op))))
               (filter (λ (n) (eq? (node-type n) 'capability)) scope)))
            roots scopes))
         #f))

;; Every op a handler body invokes outside its own `via` row.
(define (unhandled-handler-invocations-query sys)
  (query 'verify-unhandled-handler-invocations
         (λ (idx args) (handler-roots sys))
         (single-step (forward-hop 'handler-call))
         (λ (idx args roots scopes)
           (append-map
            (λ (h scope)
              (define cap (handler-cap-name idx h))
              (define row
                (remove-duplicates
                 (for/list ([n (in-list (reached idx (list h) 'handler-interprets))])
                   (car (node-name n)))))
              (for*/list ([op (in-list scope)]
                          #:when (eq? (node-type op) 'op)
                          [owner (in-value (op-owner idx (node-name op)))]
                          #:when owner
                          #:unless (member owner row))
                (type-finding
                 (unhandled-handler-invocation cap owner (node-name op)))))
            roots scopes))
         #f))

;; ── policy question ─────────────────────────────────────────────────

;; A `never`/`must` predicate is evaluated by the description-predicate query
;; (component-needs as a traversal, not/and/or composing it).
(define (policy-query sys)
  (query 'verify-policies
         (λ (idx args) (policy-roots sys))
         (no-steps)
         (λ (idx args roots scopes)
           (for*/list ([p (in-list roots)]
                       [entries (in-value (node-name p))]
                       [kind (in-value (second entries))]
                       [pred (in-value (third entries))]
                       #:when (let ([holds? (eval-description-predicate idx pred)])
                                (case kind
                                  [(never) holds?]
                                  [(must) (not holds?)]
                                  [else #f])))
             (policy-finding (policy-violation kind pred))))
         #f))


;; ── bridge question (coverage + provenance) ─────────────────────────

;; Every effect a component's host code performs that its needs do not cover
;; through the interpretation table (and the unresolvable-call note).
(define (bridge-query sys)
  (query 'verify-bridge
         (λ (idx args) (component-roots sys))
         (λ (args) (list (list (forward-hop 'component-needs))
                         (list (forward-hop 'capability-effect))))
         (λ (idx args roots scopes)
           (append-map
            (λ (c scope)
              (define allowed
                (map node-name (filter (λ (n) (eq? (node-type n) 'effect)) scope)))
              (for*/list ([used (in-list (reached idx (list c) 'fn-effects))]
                          [eff (in-value (node-name used))]
                          #:unless (member eff allowed))
                (cond
                  [(eq? eff 'unknown)
                   (bridge-finding (bridge-unverified (node-name c)))]
                  [else
                   (define sites
                     (sort
                      (for/list ([site (in-list
                                        (follow idx (list (effect-node eff))
                                                (list (hop 'effect-site 1 2))))])
                        (node-name site))
                      string<?))
                   (bridge-finding (bridge-violation (node-name c) eff sites))])))
            roots scopes))
         #t))

;; ── model questions ─────────────────────────────────────────────────

;; Normalize one structural model issue through the shared finding text.
(define (model-finding kind message)
  (first (model-findings (list (model-issue kind message)))))

;; State names some declaration provides.
(define (declared-state-names idx sys)
  (remove-duplicates
   (for/list ([n (in-list (reached idx (state-decl-roots sys) 'declares-state))])
     (node-name n))))

;; Duplicate state declarations, in declaration order. Matches the model
;; checker's `duplicates`: each repeated name is one finding.
(define (duplicate-names names)
  (remove-duplicates
   (for/list ([x (in-list names)]
              #:when (member x (cdr (member x names))))
     x)))

(define (duplicate-states-query sys)
  (query 'verify-duplicate-states
         (λ (idx args) (state-decl-roots sys))
         (no-steps)
         (λ (idx args roots scopes)
           (map (λ (n) (model-finding 'duplicate-state
                                      (format "state ~a declared more than once" n)))
                (duplicate-names
                 (map (λ (n) (second (node-name n))) roots))))
         #f))

;; Actions writing a state no declaration provides.
(define (unknown-state-writes-query sys)
  (query 'verify-unknown-state-writes
         (λ (idx args) (action-roots sys))
         (single-step (forward-hop 'action-writes))
         (λ (idx args roots scopes)
           (define declared (declared-state-names idx sys))
           (append-map
            (λ (a scope)
              (for/list ([target (in-list scope)]
                         #:when (eq? (node-type target) 'state)
                         #:unless (member (node-name target) declared))
                (model-finding
                 'unknown-state-write
                 (format "action ~a writes undeclared state ~a"
                         (node-name a) (node-name target)))))
            roots scopes))
         #f))

;; Invariants reading a state no declaration provides.
(define (unknown-state-references-query sys)
  (query 'verify-unknown-state-references
         (λ (idx args) (invariant-roots sys))
         (single-step (forward-hop 'invariant-references))
         (λ (idx args roots scopes)
           (define declared (declared-state-names idx sys))
           (append-map
            (λ (inv scope)
              (for*/list ([ref (in-list scope)]
                          #:when (eq? (node-type ref) 'ref)
                          [v (in-value (first (node-name ref)))]
                          #:unless (member v declared))
                (model-finding
                 'unknown-state-reference
                 (format "invariant references undeclared state ~a" v))))
            roots scopes))
         #f))

;; The invariant question over reachable states keeps the exact bounded
;; state-space solver as its predicate: it evaluates the invariant expression
;; in each reachable state, which is not a walk over the projected declaration
;; graph. It is still one query; a new invariant question is a new query.
(define (invariants-query sys)
  (query 'verify-invariants
         (λ (idx args) (invariant-roots sys))
         (no-steps)
         (λ (idx args roots scopes)
           (model-findings
            (filter (λ (i)
                      (memq (model-issue-kind i)
                            '(invariant-violated invariant-unverified)))
                    (check-model sys))))
         #f))

;; ── verify: run the exact query set over the merged substrate ───────

(define (verification-queries sys host-code interpretations)
  (list (unhandled-needs-query sys)
        (unhandled-invocations-query sys)
        (missing-handler-ops-query sys)
        (missing-provided-ops-query sys)
        (unhandled-handler-invocations-query sys)
        (policy-query sys)
        (duplicate-states-query sys)
        (unknown-state-writes-query sys)
        (unknown-state-references-query sys)
        (invariants-query sys)
        (bridge-query sys)))

;; Run every checker over a spec plus host code and the interpretation table,
;; by evaluating the verification queries over one merged graph.
(define (verify sys host-code interpretations)
  (define idx (make-index (system-graph+host sys host-code interpretations)))
  (append-map (λ (q) (run-query idx q '()))
              (verification-queries sys host-code interpretations)))

;; Reference path: the struct-level checkers, kept for the differential suite
;; that proves the query path returns exactly the same findings.
(define (verify-checker-findings sys host-code interpretations)
  (append (type-findings (check-type sys))
          (policy-findings (check-policy sys))
          (model-findings (check-model sys))
          (bridge-findings (check-bridge sys host-code interpretations))))

(define (verify-ok? findings)
  (not (for/or ([f (in-list findings)])
         (eq? (finding-severity f) 'hard))))

(define (verify-report findings)
  (for/list ([f (in-list findings)])
    (format "[~a:~a] ~a" (finding-checker f) (finding-kind f) (finding-message f))))

(module+ main
  (displayln "effervesce/check/verify — System 2 verify surface loaded."))

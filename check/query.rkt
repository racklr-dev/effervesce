#lang racket

;; effervesce/check/query.rkt
;;
;; System 1: fast, closed-form questions over the indexed system graph.
;; Every answer carries a confidence tag. `certain` means the value came
;; from an index lookup or a memoized transitive closure; `unknown` means
;; the question cannot be answered from the provided graph and the caller
;; should escalate to System 2 (verify) or supply another artifact.
;;
;; System 1 never guesses: it returns `unknown` rather than an empty or
;; partial answer that could be misread as a negative result.
;;
;; Query unification: a question is DATA, not a branch in a dispatcher. A
;; `query` anchors at one or more nodes (`select`), scopes to the subgraph
;; reachable from them along declared edge kinds (`steps`, from
;; check/scope.rkt), and reduces that scope to an answer (`collect`). The
;; named questions below are registered in `query-registry`; `ask` looks the
;; question up by name. Adding a question is adding a query (see
;; `register-query!`), not editing the dispatcher — and the same engine is
;; what `verify` uses for exact questions.

(require racklr/uir
         racklr/hypergraph
         "graph.rkt"
         "scope.rkt"
         "util.rkt")

(provide
  answer answer? answer-value answer-confidence answer-note
  certain unknown
  ask
  query query? query-name query-select query-steps query-collect query-host?
  query-registry register-query! find-query run-query
  single-step no-steps
  capabilities-of effects-of side-effects-of touches resources-of
  reachable? reachable-caps
  calls-of
  graph-component-needs? eval-description-predicate
  format-ref-key
  host-level-question?)

;; ── answer ──────────────────────────────────────────────────────────

;; confidence is 'certain | 'unknown. `note` explains an `unknown`.
(struct answer (value confidence note) #:transparent)

(define (certain v) (answer v 'certain #f))
(define (unknown v note) (answer v 'unknown note))

;; ── queries ─────────────────────────────────────────────────────────

;; A query is a typed traversal plan over the merged graph:
;;
;;   name    the question's symbol, e.g. 'capabilities-of
;;   select  (idx args -> (listof node)): the anchors the question starts at
;;   steps   (args -> (listof (listof hop))): the declared edge kinds to scope
;;           along, one hop set per step (from check/scope.rkt)
;;   collect (idx args roots scopes -> value): how the scoped subgraph answers
;;           the question; `scopes` holds one reachable-node list per root
;;   host?   true when the answer needs host artifacts (fn-effects, sites)
;;           beyond a bare .fizz spec, so the CLI can report `unknown`
(struct query (name select steps collect host?) #:transparent)

(define query-registry (make-hasheq))

(define (register-query! q)
  (hash-set! query-registry (query-name q) q)
  q)

(define (find-query name)
  (hash-ref query-registry name #f))

;; Evaluate one query: anchor, scope along the declared edge kinds, collect.
(define (run-query idx q args)
  (define roots ((query-select q) idx args))
  (define steps ((query-steps q) args))
  (define scopes (for/list ([r (in-list roots)])
                   (scope-nodes idx (list r) steps)))
  ((query-collect q) idx args roots scopes))

;; The result of a registered question, for the exported functions below.
(define (query-value name idx . args)
  (run-query idx (find-query name) args))

;; ── answer helpers ──────────────────────────────────────────────────

(define (reached-of-nodes scopes type)
  (remove-duplicates
   (for*/list ([scope (in-list scopes)] [n (in-list scope)]
               #:when (eq? (node-type n) type))
     (node-name n))))

;; The common step shape: one hop set holding one declared edge kind.
(define (single-step hop)
  (λ (args) (list (list hop))))

(define (no-steps) (λ (args) '()))

;; Anchor a question at the named node kind (callers pass the name as the
;; question's single argument).
(define (anchored node-maker)
  (λ (idx args) (if (null? args) '() (list (node-maker (first args))))))

;; ── closed-form questions (registered queries) ──────────────────────

;; Capabilities a component `needs` (list of ref-keys).
(register-query!
 (query 'capabilities-of
        (anchored component-node)
        (single-step (forward-hop 'component-needs))
        (λ (idx args roots scopes) (reached-of-nodes scopes 'capability))
        #f))

;; Host effects inferred for a component's code (list of effect-name symbols).
(register-query!
 (query 'effects-of
        (anchored component-node)
        (single-step (forward-hop 'fn-effects))
        (λ (idx args roots scopes) (reached-of-nodes scopes 'effect))
        #t))

;; Effects that touch a given resource/scope (the DualLink inverse).
(register-query!
 (query 'touches
        (anchored resource-node)
        (single-step (backward-hop 'effect-touches))
        (λ (idx args roots scopes) (reached-of-nodes scopes 'effect))
        #t))

;; Resources an effect touches.
(register-query!
 (query 'resources-of
        (anchored effect-node)
        (single-step (forward-hop 'effect-touches))
        (λ (idx args roots scopes) (reached-of-nodes scopes 'resource))
        #t))

;; ── handling-graph questions ────────────────────────────────────────
;;
;; The handling graph is the capability graph reachable from a capability:
;; `via` interpretations forward, peer providers backward to their components,
;; then those components' `needs` forward. Side effects and reachability are
;; both scoped traversals over it; handledness needs the and/or shape of a
;; `via` row, so it keeps its memoized evaluation.

;; The declared edge kinds a capability-handling scope may walk.
(define (handling-hops)
  (list (forward-hop 'capability-interprets)
        (backward-hop 'component-provides)
        (forward-hop 'component-needs)))

;; Is capability `cap` (a ref-key) handled: does a path exist through `via`
;; interpretations and peer `provides` down to a `given`? Memoized DFS with
;; per-path cycle detection. Matches the type checker's `handled?` for the
;; single-handler-per-capability case the examples use.
(define (reachable? idx cap)
  (define h (handling-structure idx))
  (define memo (make-hash))
  (define visiting (make-hash))
  (define (go c)
    (cond
      [(hash-ref visiting c #f) #f]              ; cycle on this path
      [(hash-has-key? memo c) (hash-ref memo c)]
      [else
       (hash-set! visiting c #t)
       (define ts (hash-ref (handling-via-targets h) c '()))
       (define result
         (or (set-member? (handling-givens h) c)
             (and (pair? ts) (andmap go ts))
             (for/or ([p (in-list (hash-ref (handling-providers-by-key h) c '()))])
               (andmap go (hash-ref (handling-needs-of h) p '())))))
       (hash-remove! visiting c)
       (hash-set! memo c result)
       result]))
  (go cap))

;; The capability-handling graph, indexed for the memoized question. Mirrors
;; the type checker's `given-keys` / `via-edges` / `providers-by-key` /
;; `provider-needs`.
(struct handling (givens via-targets providers-by-key needs-of) #:transparent)

(define (edge-map idx type kpos vpos)
  (define h (make-hash))
  (for ([l (in-list (index-links idx type))])
    (hash-update! h (node-name (list-ref (link-out l) kpos))
                  (λ (vs) (cons (node-name (list-ref (link-out l) vpos)) vs))
                  '()))
  h)

(define (handling-structure idx)
  (handling
   (list->set
    (map (λ (l) (node-name (first (link-out l)))) (index-links idx 'given)))
   (edge-map idx 'capability-interprets 0 1)
   (edge-map idx 'component-provides 1 0)
   (edge-map idx 'component-needs 0 1)))

;; Every capability on a handling path from `cap`, including `cap` itself.
;; This is the plain closure of the handling edge kinds, so it is a scope
;; traversal like any other (the and/or of a `via` row only matters for
;; reachable?). Values are ref-keys because Racket sets are used.
(register-query!
 (query 'reachable-caps
        (λ (idx args) (list (node 'capability (first args))))
        (no-steps)
        (λ (idx args roots scopes)
          (if (null? roots)
              (list (first args))
              (remove-duplicates
               (for/list ([n (in-list (closure idx roots (handling-hops)))]
                          #:when (eq? (node-type n) 'capability))
                 (node-name n)))))
        #f))

;; Is the capability handled? (boolean answer, not a list).
(register-query!
 (query 'reachable?
        (λ (idx args) (list (node 'capability (first args))))
        (no-steps)
        (λ (idx args roots scopes)
          (and (pair? roots) (reachable? idx (node-name (first roots)))))
        #f))

;; Side effects a component performs: its own inferred effects, plus the
;; effects of every capability on its handling paths, one hop through the
;; interpretation table.
(register-query!
 (query 'side-effects-of
        (anchored component-node)
        (no-steps)
        (λ (idx args roots scopes)
          (if (null? roots)
              '()
              (let* ([needed (follow idx roots (list (forward-hop 'component-needs)))]
                     [closure-caps (closure idx needed (handling-hops))]
                     [direct (follow idx roots (list (forward-hop 'fn-effects)))]
                     [via (follow idx closure-caps (list (forward-hop 'capability-effect)))])
                (remove-duplicates
                 (append (map node-name direct)
                         (remove-duplicates (map node-name via)))))))
        #t))

;; ── materialized AST questions ──────────────────────────────────────

;; The atom at slot position `i` of a materialized vertex (one slot link hop).
(define (slot-child idx vertex i)
  (for/or ([l (in-list (index-links idx 'slot))]
           #:when (equal? (first (link-out l)) vertex))
    (and (= (node-name (second (link-out l))) i)
         (third (link-out l)))))

;; Resolve a materialized vertex to its leaf scalar (following slot 0 down
;; through single-child wrappers like uir-symbol/uir-var), or #f.
(define (leaf-of idx a)
  (let loop ([a a] [depth 0])
    (cond
      [(and (node? a) (memq (node-type a) '(sym str num bool nil)))
       (node-name a)]
      [(and (node? a) (< depth 64))
       (define c (slot-child idx a 0))
       (and c (loop c (add1 depth)))]
      [else #f])))

;; The callee name of one materialized uir-call vertex, if it names one.
(define (call-vertex-name idx cv)
  (define callee (slot-child idx cv 0))
  (cond
    [(and (node? callee) (memq (node-type callee) '(uir-symbol uir-var)))
     (leaf-of idx callee)]
    [(and (node? callee) (eq? (node-type callee) 'uir-get))
     (leaf-of idx (slot-child idx callee 1))]
    [else #f]))

;; Every operation/function name invoked by materialized host ASTs. The
;; anchors are the uir-call vertices; the collector resolves each one's
;; callee through the materialized slots.
(define calls-of
  (λ (idx)
    (remove-duplicates
     (filter values
             (for/list ([cv (in-list (index-nodes idx 'uir-call))])
               (call-vertex-name idx cv))))))

;; ── generic scoping question ────────────────────────────────────────

;; (ask idx (list 'scope-of node-type node-name edge-kind direction)) scopes
;; from the anchored node along one declared edge kind. This is the generic
;; form every named question below could be expressed with; registering it is
;; the extension point a new question uses.
(define (scope-of-hop args)
  (match args
    [(list type name edge direction)
     (case direction
       [(forward) (forward-hop edge)]
       [(backward) (backward-hop edge)]
       [else (error 'scope-of "unknown direction: ~a" direction)])]
    [_ (error 'scope-of "expected (node-type name edge-kind direction)")]))

;; Asks for the context of one anchor: the nodes reachable from it along one
;; declared edge kind. The anchor itself is the question, not the answer.
(register-query!
 (query 'scope-of
        (λ (idx args)
          (match args
            [(list type name edge direction) (list (node type name))]
            [_ '()]))
        (no-steps)
        (λ (idx args roots scopes)
          (map node-name (remove-duplicates (follow idx roots (list (scope-of-hop args))))))
        #f))

;; ── dispatcher ──────────────────────────────────────────────────────

;; `q` is a structured question: (list 'capabilities-of cname), etc. Every
;; question resolves through the registry, so an unknown name is `unknown`.
(define (ask idx q)
  (match q
    [(cons name args)
     (define qy (find-query name))
     (if qy
         (certain (run-query idx qy args))
         (unknown 'unsupported (format "no such query: ~a" q)))]
    [_ (unknown 'unsupported (format "no such query: ~a" q))]))

;; Questions that need host artifacts (fn-effects, effect-touches) beyond a
;; bare .fizz spec. The CLI reports these as unknown when only a spec is given.
(define (host-level-question? q)
  (and (pair? q)
       (let ([qy (find-query (car q))])
         (and qy (query-host? qy) #t))))

;; ── exported question functions ─────────────────────────────────────

(define (capabilities-of idx cname) (query-value 'capabilities-of idx cname))
(define (effects-of idx cname) (query-value 'effects-of idx cname))
(define (side-effects-of idx cname) (query-value 'side-effects-of idx cname))
(define (touches idx resource) (query-value 'touches idx resource))
(define (resources-of idx effect) (query-value 'resources-of idx effect))
(define (reachable-caps idx cap)
  (list->set (query-value 'reachable-caps idx cap)))

;; ── description predicates ──────────────────────────────────────────
;;
;; `never` / `must` predicates range over the description, not runtime
;; values. Their atoms are graph questions too: `component needs R` is an
;; existence query over component-needs edges, and not/and/or compose it.

(define (graph-component-needs? idx ref)
  (define key (ref-key ref))
  (for/or ([cap (in-list (follow idx (index-nodes idx 'component)
                                 (list (forward-hop 'component-needs))))])
    (equal? (node-name cap) key)))

(define (eval-description-predicate idx p)
  (cond
    [(uir-paren? p) (eval-description-predicate idx (uir-paren-inner p))]
    [(uir-call? p)
     (define op (uir-symbol-name (uir-call-callee p)))
     (define args (uir-call-args p))
     (define a (and (pair? args) (eval-description-predicate idx (first args))))
     (define b (and (pair? args) (pair? (cdr args))
                    (eval-description-predicate idx (second args))))
     (cond
       [(equal? op "component-needs") (graph-component-needs? idx (first args))]
       [(equal? op "not") (not a)]
       [(equal? op "and") (and a b)]
       [(equal? op "or") (or a b)]
       [else #f])]
    [else #f]))

;; ── formatting ──────────────────────────────────────────────────────

(define (format-ref-key k)
  (define name (car k))
  (define args (cdr k))
  (if (null? args)
      name
      (string-append name "<" (string-join args ", ") ">")))

(module+ main
  (displayln "effervesce/check/query — System 1 query surface loaded."))

#lang racket

;; Contextual scoping tests: a query anchors at a node and scopes to the
;; subgraph reachable from it along declared edge kinds. Also the extension
;; contract: a new question is a new query, registered without touching the
;; dispatcher.

(require rackunit
         racklr/uir
         racklr/hypergraph
         "../load.rkt"
         "graph.rkt"
         "scope.rkt"
         "query.rkt")

;; Peer-provider wiring: App -> Store -> Database -> Db -> given.
(define spec
  (load-effervesce
   "system S {
      capability Store { add(text: String) -> Todo }
      capability Db { insert(text: String) -> Unit }
      given Db
      component Database needs Db provides Store { add(text) = insert(text) }
      component App needs Store { addTodo(text) = add(text) }
    }"))

(define idx (make-index (system-graph spec)))

(define (cap-key name) (cons name '()))
(define (cap-node name) (capability-node (uir-eff-ref (uir-symbol name) '())))

;; ── declared edge kinds ─────────────────────────────────────────────

(define (test-declared-edges)
  (check-true (edge-declared? 'component-needs 0 1))
  (check-true (edge-declared? 'component-needs 1 0))
  (check-false (edge-declared? 'component-needs 0 2))
  (check-false (edge-declared? 'no-such-edge 0 1))
  ;; A hop only names a declared edge kind/slot pair.
  (check-exn exn:fail? (λ () (hop 'no-such-edge 0 1)))
  (check-exn exn:fail? (λ () (hop 'component-needs 0 2)))
  (check-true (hop? (forward-hop 'component-needs)))
  (check-equal? (hop-type (backward-hop 'component-needs)) 'component-needs))

;; ── anchored traversal ──────────────────────────────────────────────

(define (test-follow)
  ;; One hop forward from the component anchor: it needs Store.
  (define caps (follow idx (list (component-node "App"))
                       (list (forward-hop 'component-needs))))
  (check-equal? (map node-name caps) (list (cap-key "Store")))
  ;; The inverse direction: Store is provided by Database.
  (define providers (follow idx (list (cap-node "Store"))
                            (list (backward-hop 'component-provides))))
  (check-equal? (map node-name providers) (list "Database")))

(define (test-traverse-and-scope)
  ;; Two declared steps: App needs Store, and Store is provided by Database.
  (define plan (list (list (forward-hop 'component-needs))
                     (list (backward-hop 'component-provides))))
  (define reached (traverse idx (list (component-node "App")) plan))
  ;; scope-nodes keeps the anchor plus everything reachable along the plan.
  (define scope (scope-nodes idx (list (component-node "App")) plan))
  (check-true (set-member? reached (component-node "Database")))
  (check-not-false (member (component-node "Database") scope))
  ;; The scope keeps the intermediate level too: Store is on the path.
  (check-not-false (member (cap-node "Store") scope))
  ;; The induced subgraph carries the links the walk crossed, not just nodes.
  (define atoms (scope-atoms idx (list (component-node "App"))
                             (list (list (forward-hop 'component-needs)))))
  (check-not-false (memq 'component-needs (map link-type (filter link? atoms))))
  (check-not-false (member (component-node "App") atoms)))

;; ── handling-graph closure ──────────────────────────────────────────

(define (test-closure)
  ;; Store's handling path reaches Database's Db need and Db itself.
  (define caps (closure idx (list (cap-node "Store"))
                        (list (forward-hop 'capability-interprets)
                              (backward-hop 'component-provides)
                              (forward-hop 'component-needs))))
  (check-not-false (member (cap-node "Store") caps))
  (check-not-false (member (cap-node "Db") caps))
  ;; The same scope answers the closed-form question.
  (check-true (reachable? idx (cap-key "Store")))
  (check-false (reachable? idx (cap-key "Missing"))))

;; ── provenance paths ────────────────────────────────────────────────

(define (test-walk-paths)
  (define paths (walk-paths idx (list (component-node "App"))
                            (list (list (forward-hop 'component-needs)))))
  ;; App -> Store, the exact edge that explains the needs relation.
  (check-true (for/or ([p (in-list paths)])
                (equal? (map node-name p) (list "App" (cap-key "Store"))))))

;; ── the generic anchored question ───────────────────────────────────

(define (test-scope-of-question)
  (check-equal? (answer-value
                 (ask idx (list 'scope-of 'component "App" 'component-needs 'forward)))
                (list (cap-key "Store")))
  (check-equal? (answer-value
                 (ask idx (list 'scope-of 'capability (cap-key "Store")
                                'component-provides 'backward)))
                (list "Database"))
  ;; An undeclared edge kind is a typed error, never a silent empty walk.
  (check-exn exn:fail?
             (λ () (ask idx (list 'scope-of 'component "App" 'no-such-edge 'forward)))))

;; ── a new question is a new query ───────────────────────────────────

(define (test-new-question-is-a-new-query)
  ;; Adding a question: anchor at a capability, scope backward over
  ;; component-provides, collect component names. No dispatcher change.
  (register-query!
   (query 'providers-of
          (λ (idx args) (list (node 'capability (first args))))
          (single-step (backward-hop 'component-provides))
          (λ (idx args roots scopes)
            (remove-duplicates
             (for/list ([n (in-list (first scopes))]
                        #:when (eq? (node-type n) 'component))
               (node-name n))))
          #f))
  (check-equal? (answer-value (ask idx (list 'providers-of (cap-key "Store"))))
                (list "Database"))
  ;; Registering the same name replaces the question; unregister to leave the
  ;; shared registry as found.
  (hash-remove! query-registry 'providers-of)
  (check-equal? (answer-confidence (ask idx (list 'providers-of (cap-key "Store"))))
                'unknown))

(module+ main
  (test-declared-edges)
  (test-follow)
  (test-traverse-and-scope)
  (test-closure)
  (test-walk-paths)
  (test-scope-of-question)
  (test-new-question-is-a-new-query)
  (displayln "All scope tests passed."))

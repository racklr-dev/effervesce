#lang racket

;; System-graph projection tests: does the hypergraph view of a .fizz spec
;; (plus host code and the interpretation table) contain exactly the nodes
;; and links the query layer will need?

(require rackunit
         racklr/uir
         racklr/hypergraph
         "../load.rkt"
         "graph.rkt")

(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))

(define spec
  (load-effervesce
   "system S {
      capability Db { insert(text: String) -> Unit }
      capability Store { add(text: String) -> Todo }
      given Db
      Store via Db { add(text) = insert(text) }
      state cache: Todo = none
      action Load { cache := readTodo() }
      component Api needs Store { addTodo(text) = add(text) }
    }"))

;; ── spec projection ─────────────────────────────────────────────────

(define g (system-graph spec))
(define idx (make-index g))

(define (has-node? type name) (not (not (index-node idx type name))))

(define (test-spec-nodes)
  (check-true (has-node? 'component "Api"))
  (check-true (has-node? 'capability (cons "Db" '())))
  (check-true (has-node? 'capability (cons "Store" '())))
  (check-true (has-node? 'state "cache"))
  (check-true (has-node? 'action "Load")))

(define (test-spec-links)
  ;; Api needs Store
  (define needs (index-links idx 'component-needs))
  (check-equal? (length needs) 1)
  (check-equal? (node-name (first (link-out (first needs)))) "Api")
  (check-equal? (node-name (second (link-out (first needs)))) (cons "Store" '()))

  ;; Store via Db
  (define via (index-links idx 'capability-interprets))
  (check-equal? (length via) 1)
  (check-equal? (node-name (first (link-out (first via)))) (cons "Store" '()))
  (check-equal? (node-name (second (link-out (first via)))) (cons "Db" '()))

  ;; Db is given (terminal)
  (define given (index-links idx 'given))
  (check-equal? (length given) 1)
  (check-equal? (node-name (first (link-out (first given)))) (cons "Db" '()))

  ;; action Load writes state cache
  (define writes (index-links idx 'action-writes))
  (check-equal? (length writes) 1)
  (check-equal? (node-name (first (link-out (first writes)))) "Load")
  (check-equal? (node-name (second (link-out (first writes)))) "cache"))

;; ── host projection ─────────────────────────────────────────────────

(define host
  (hash "Api" (prog (fn "handler" (uir-call (dot (v "db") "execute") '())))))

(define interp (hash "Db" '(db)))

(define g2 (system-graph+host spec host interp))
(define idx2 (make-index g2))

(define (test-host-links)
  ;; Api's inferred host effect is db
  (define fe (index-links idx2 'fn-effects))
  (check-equal? (length fe) 1)
  (check-equal? (node-name (first (link-out (first fe)))) "Api")
  (check-equal? (node-name (second (link-out (first fe)))) 'db)

  ;; the interpretation table became capability-effect edges
  (define ce (index-links idx2 'capability-effect))
  (check-equal? (length ce) 1)
  (check-equal? (node-name (first (link-out (first ce)))) (cons "Db" '()))
  (check-equal? (node-name (second (link-out (first ce)))) 'db))

;; ── recovered capability graph for undeclared host components ──────

(define (test-recovered-host-links)
  ;; The spec declares Api; Worker is host-only. Recovery fills Worker's
  ;; capability-level links from its inferred rows and leaves Api's declared
  ;; capability graph authoritative.
  (define progs
    (hash "Api" host
          "Worker" (prog (fn "job" (uir-call (dot (v "db") "execute") '())))))
  (define g3 (system-graph+host spec progs interp))
  (define idx3 (make-index g3))
  (define (needs-of c)
    (for/list ([l (in-list (index-links idx3 'component-needs))]
               #:when (equal? (node-name (first (link-out l))) c))
      (node-name (second (link-out l)))))
  (check-equal? (needs-of "Api") (list (cons "Store" '())))
  (check-equal? (needs-of "Worker") (list (cons 'db '())))
  ;; Worker's coarse db entry is interpreted as the db effect
  (check-true
   (for/or ([l (in-list (index-links idx3 'capability-effect))])
     (equal? (map node-name (link-out l)) (list (cons 'db '()) 'db)))))

;; ── link constructors, exercised directly ───────────────────────────

(define (test-constructors)
  (check-equal? (effect-touches-link 'db "reads=x")
                (link 'effect-touches
                      (list (effect-node 'db) (resource-node "reads=x"))))
  (check-equal? (component-provides-link "Database"
                                         (uir-eff-ref (uir-symbol "Store") '()))
                (link 'component-provides
                      (list (component-node "Database")
                            (capability-node (uir-eff-ref (uir-symbol "Store") '()))))))

(module+ main
  (test-spec-nodes)
  (test-spec-links)
  (test-host-links)
  (test-recovered-host-links)
  (test-constructors)
  (displayln "All graph tests passed."))

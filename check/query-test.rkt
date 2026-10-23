#lang racket

;; System 1 query tests: capabilities, side effects, reachability, and the
;; confidence contract. Reachability must agree with the type checker's
;; `given`/`via`/`provides` handling.

(require rackunit
         racklr/uir
         racklr/hypergraph
         "../load.rkt"
         "graph.rkt"
         "query.rkt")

(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))

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

(define g (system-graph spec))
(define idx (make-index g))

(define (test-capabilities-of)
  (check-equal? (capabilities-of idx "App") (list (cons "Store" '())))
  (check-equal? (capabilities-of idx "Database") (list (cons "Db" '())))
  (check-equal? (capabilities-of idx "Nobody") '()))

(define (test-reachable)
  (check-true (reachable? idx (cons "Db" '())))
  (check-true (reachable? idx (cons "Store" '())))
  (check-false (reachable? idx (cons "Missing" '()))))

(define (test-reachable-caps)
  ;; Store's handling path reaches itself, Database's Db need, and Db.
  (define caps (reachable-caps idx (cons "Store" '())))
  (check-true (set-member? caps (cons "Store" '())))
  (check-true (set-member? caps (cons "Db" '()))))

(define (test-unhandled-is-unknown-not-false)
  ;; An unsupported question returns unknown, not a guessed answer.
  (define a (ask idx (list 'who-knows "x")))
  (check-equal? (answer-confidence a) 'unknown))

(define (test-ask-dispatcher)
  (check-equal? (answer-value (ask idx (list 'capabilities-of "App")))
                (list (cons "Store" '())))
  (check-true (answer-value (ask idx (list 'reachable? (cons "Db" '())))))
  (check-equal? (answer-confidence (ask idx (list 'reachable? (cons "Db" '()))))
                'certain))

;; ── host-side queries ───────────────────────────────────────────────

(define host
  (hash "App" (prog (fn "handler" (uir-call (dot (v "db") "execute") '())))))

(define interp (hash "Db" '(db)))

(define g2 (system-graph+host spec host interp))
(define idx2 (make-index g2))

(define (test-effects-and-side-effects)
  ;; App's own inferred effect is db.
  (check-equal? (effects-of idx2 "App") '(db))
  ;; Side effects close over the handling path: App needs Store, which is
  ;; provided by Database, which needs Db, which interprets to db.
  (check-equal? (side-effects-of idx2 "App") '(db)))

(define (test-touches-constructors)
  (define g3 (make-index (list (effect-touches-link 'db "reads=x"))))
  (check-equal? (touches g3 "reads=x") '(db))
  (check-equal? (resources-of g3 'db) '("reads=x")))

(define (test-format-ref-key)
  (check-equal? (format-ref-key (cons "Store" '())) "Store")
  (check-equal? (format-ref-key (cons "Read" '("Config"))) "Read<Config>"))

(module+ main
  (test-capabilities-of)
  (test-reachable)
  (test-reachable-caps)
  (test-unhandled-is-unknown-not-false)
  (test-ask-dispatcher)
  (test-effects-and-side-effects)
  (test-touches-constructors)
  (test-format-ref-key)
  (displayln "All query tests passed."))

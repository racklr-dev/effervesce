#lang racket

;; Library-surface tests: the three verbs over .fizz text, with the
;; System 1 -> System 2 escalation exercised as a caller would.

(require rackunit
         racklr/uir
         "library.rkt")

(define spec-text
  "system S {
     capability Store { add(text: String) -> Todo }
     capability Db { insert(text: String) -> Unit }
     given Db
     component Database needs Db provides Store { add(text) = insert(text) }
     component App needs Store { addTodo(text) = add(text) }
   }")

(define (test-analyze-and-ask)
  (define idx (analyze spec-text))
  (define a (ask idx '(capabilities-of "App")))
  (check-equal? (answer-confidence a) 'certain)
  (check-equal? (answer-value a) (list (cons "Store" '())))
  (check-true (answer-value (ask idx '(reachable? ("Store")))))
  (check-false (answer-value (ask idx '(reachable? ("Missing"))))))

(define (test-verify-clean)
  (check-true (verify-ok? (verify spec-text))))

(define (test-verify-catches-hole)
  (define leaky-text
    "system S {
       capability Store { add(text: String) -> Todo }
       component App needs Store { addTodo(text) = add(text) }
     }")
  (define fs (verify leaky-text))
  (check-false (verify-ok? fs))
  (check-true (for/or ([f (in-list fs)])
                (and (eq? (finding-checker f) 'type)
                     (eq? (finding-kind f) 'unhandled-need)))))

(define (test-escalation-path)
  ;; System 1 answers, then System 2 escalates the same system.
  (define idx (analyze spec-text))
  (define certain-cap (ask idx '(capabilities-of "App")))
  (check-equal? (answer-confidence certain-cap) 'certain)
  (check-true (verify-ok? (verify spec-text))))

(module+ main
  (test-analyze-and-ask)
  (test-verify-clean)
  (test-verify-catches-hole)
  (test-escalation-path)
  (displayln "All library tests passed."))

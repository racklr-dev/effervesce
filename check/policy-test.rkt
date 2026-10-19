#lang racket

(require rackunit
         racket/runtime-path
         racklr/uir
         "../load.rkt"
         "policy.rkt")

(define-runtime-path examples-dir "../examples")

(define (load-file name)
  (load-effervesce (file->string (build-path examples-dir name))))

;; The shipped examples' policies all hold.
(check-true (policy-check-ok? (load-file "todo-rules.fizz")))
(check-true (policy-check-ok? (load-file "config-reader.fizz")))

;; `must component needs Missing` fails: no component needs Missing.
(define must-missing
  (load-effervesce
   "system P {
      capability Store { add() -> Unit }
      given Store
      component App needs Store { run() = add() }
      must component needs Missing
    }"))
(check-equal? (length (check-policy must-missing)) 1)
(check-equal? (policy-violation-kind (first (check-policy must-missing))) 'must)

;; `never component needs Store` fails when App needs Store.
(define never-store
  (load-effervesce
   "system Q {
      capability Store { add() -> Unit }
      given Store
      component App needs Store { run() = add() }
      never component needs Store
    }"))
(check-equal? (length (check-policy never-store)) 1)
(check-equal? (policy-violation-kind (first (check-policy never-store))) 'never)

;; `component needs Read` matches a component that needs Read<Config> by name.
(define named-ref
  (load-effervesce
   "system R {
      capability Read<A> { get() -> A }
      given Read
      component App needs Read<Config> { run() = get() }
      must component needs Read
    }"))
(check-true (policy-check-ok? named-ref))

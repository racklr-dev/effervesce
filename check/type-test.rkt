#lang racket

(require rackunit
         racket/runtime-path
         racklr/uir
         "type.rkt"
         "../load.rkt")

(define-runtime-path examples-dir "../examples")

(define (example-sys name)
  (load-effervesce (file->string (build-path examples-dir name))))

(define (check-file-ok? name)
  (check-true (type-check-ok? (example-sys name))
              (format "type check ~a" name)))

(check-file-ok? "config-reader.fizz")
(check-file-ok? "todo-store.fizz")
(check-file-ok? "todo-memory.fizz")
(check-file-ok? "todo-database.fizz")
(check-file-ok? "todo-rules.fizz")

;; A bare unhandled need.
(define unhandled
  (load-effervesce
   "system Unhandled {
      capability Store { add() -> Unit }
      component App needs Store { addTodo() = add() }
    }"))
(check-equal? (length (check-type unhandled)) 1)
(check-equal? (unhandled-need-component (first (check-type unhandled))) "App")
(check-equal? (uir-symbol-name
               (uir-eff-ref-name (unhandled-need-capability (first (check-type unhandled)))))
              "Store")

;; `via` only satisfies a need if its own row is satisfiable.
(define broken-via
  (load-effervesce
   "system Broken {
      capability A { x() -> Unit }
      capability B { y() -> Unit }
      B via A { y() = x() }
      component C needs B { run() = y() }
    }"))
(check-equal? (length (check-type broken-via)) 1)

;; A via cycle terminates and counts as unhandled.
(define cycle
  (load-effervesce
   "system Cycle {
      capability A { x() -> Unit }
      capability B { y() -> Unit }
      A via B { x() = y() }
      B via A { y() = x() }
      component C needs A { run() = x() }
    }"))
(check-equal? (length (check-type cycle)) 1)

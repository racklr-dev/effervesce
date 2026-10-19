#lang racket

(require (except-in rackunit check)
         racket/runtime-path
         racklr/uir
         "../check.rkt")

(define-runtime-path examples-dir "../examples")

(define (example-path name) (build-path examples-dir name))

;; An incomplete system reports an unhandled capability and is not ok.
(define incomplete (check-file (example-path "todo-incomplete.fizz")))
(check-false (check-ok? incomplete))
(check-true
 (for/or ([m (in-list (check-report incomplete))])
   (regexp-match? #rx"unhandled capability" m)))

;; The complete examples all check clean.
(check-true (check-ok? (check-file (example-path "todo-database.fizz"))))
(check-true (check-ok? (check-file (example-path "todo-rules.fizz"))))
(check-true (check-ok? (check-file (example-path "config-reader.fizz"))))
(check-true (check-ok? (check-file (example-path "todo-memory.fizz"))))

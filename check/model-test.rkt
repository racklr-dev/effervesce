#lang racket

(require rackunit
         racket/runtime-path
         racklr/uir
         "../load.rkt"
         "model.rkt")

(define-runtime-path examples-dir "../examples")

(define (load-file name)
  (load-effervesce (file->string (build-path examples-dir name))))

(define (has-kind? issues kind)
  (for/or ([i (in-list issues)])
    (eq? (model-issue-kind i) kind)))

;; todo-rules has one invariant referencing uninterpreted ops: informational,
;; not a failure.
(define tr (load-file "todo-rules.fizz"))
(check-true (model-check-ok? tr))
(check-equal? (length (check-model tr)) 1)

;; A statically-true invariant discharges with no issues.
(define holds (load-effervesce "system M { always true }"))
(check-true (model-check-ok? holds))
(check-equal? (length (check-model holds)) 0)

;; A statically-false invariant is a violation.
(define violated (load-effervesce "system M { always false }"))
(check-false (model-check-ok? violated))
(check-true (has-kind? (check-model violated) 'invariant-violated))

;; Numeric comparison over literals evaluates.
(define cmp (load-effervesce "system M { always 1 > 2 }"))
(check-false (model-check-ok? cmp))

;; An action writing an undeclared state is flagged.
(define bad-write (load-effervesce "system M { state s: Int = 0 action A { t := 1 } }"))
(check-false (model-check-ok? bad-write))
(check-true (has-kind? (check-model bad-write) 'unknown-state-write))

;; An invariant referencing an undeclared state is flagged.
(define bad-read (load-effervesce "system M { always x == 1 }"))
(check-false (model-check-ok? bad-read))
(check-true (has-kind? (check-model bad-read) 'unknown-state-reference))

;; A state declared twice is flagged.
(define dup (load-effervesce "system M { state s: Int = 0 state s: Int = 1 }"))
(check-false (model-check-ok? dup))
(check-true (has-kind? (check-model dup) 'duplicate-state))

;; A convergent transition discharges its invariant (no unverified note).
(define proven
  (load-effervesce "system M { state s: Int = 0 action Set { s := 1 } always s <= 1 }"))
(check-true (model-check-ok? proven))
(check-equal? (length (check-model proven)) 0)

;; The shipped todo-started.fizz proves clean: a bounded flag transition.
(check-true (model-check-ok? (load-file "todo-started.fizz")))
(check-equal? (length (check-model (load-file "todo-started.fizz"))) 0)

;; A reachable violation (decrementing below zero) is caught.
(define overflow
  (load-effervesce "system M { state s: Int = 3 action Dec { s := s - 1 } always s >= 0 }"))
(check-false (model-check-ok? overflow))
(check-true (has-kind? (check-model overflow) 'invariant-violated))

;; `none` is a concrete null value: a state initialized to `none` and compared
;; against `none` discharges.
(define null-init
  (load-effervesce "system M { state s: Todo = none always s == none }"))
(check-true (model-check-ok? null-init))
(check-equal? (length (check-model null-init)) 0)

;; `%` modulo evaluates.
(define mod-expr
  (load-effervesce "system M { always 7 % 3 == 1 }"))
(check-true (model-check-ok? mod-expr))
(check-equal? (length (check-model mod-expr)) 0)

;; String concatenation via `+` evaluates.
(define str-cat
  (load-effervesce "system M { always \"a\" + \"b\" == \"ab\" }"))
(check-true (model-check-ok? str-cat))
(check-equal? (length (check-model str-cat)) 0)

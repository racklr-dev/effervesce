#lang racket

;; Bridge checker: inferred host-code effect rows vs. a component's declared
;; `needs` (interpreted through a capability-interpretation table).

(require rackunit
         racklr/uir
         "../load.rkt"
         "bridge.rkt")

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))

;; A component whose code only performs the effect its needs cover.
(define spec
  (load-effervesce
   "system S {
      capability Db { query() -> Unit }
      given Db
      component Api needs Db { run() = query() }
    }"))

(define clean-code
  (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))))

(check-equal?
 (check-bridge spec (hash "Api" clean-code) (hash "Db" '(db)))
 '())

;; A component whose code performs an effect nothing in its needs covers.
(define leaky-code
  (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))
        (fn "fetcher" (uir-call (v "fetch") '()))))

(define leaky
  (check-bridge spec (hash "Api" leaky-code) (hash "Db" '(db))))
(check-equal? (length leaky) 1)
(check-true (bridge-violation? (first leaky)))
(check-equal? (bridge-violation-component (first leaky)) "Api")
(check-equal? (bridge-violation-effect (first leaky)) 'network)

;; An unresolvable (higher-order) call is reported unverified, not a violation.
(define unknown-code
  (prog (fn "h" (uir-call (v "callback") '()))))

(define unk
  (check-bridge spec (hash "Api" unknown-code) (hash "Db" '(db))))
(check-equal? (length unk) 1)
(check-true (bridge-unverified? (first unk)))
(check-equal? (bridge-unverified-component (first unk)) "Api")

;; Components with no host code are skipped, not flagged.
(check-equal? (check-bridge spec (hash) (hash "Db" '(db))) '())

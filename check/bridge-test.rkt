#lang racket

;; Bridge checker: inferred host-code effect rows vs. a component's declared
;; `needs` (interpreted through a capability-interpretation table).

(require rackunit
         racklr/uir
         racklr/effects/algebra
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

;; A higher-order (bare callback) call is a `dynamic` violation when the
;; component's needs do not cover `dynamic` (mirrors the declared-check rule).
(define higher-order-code
  (prog (fn "h" (uir-call (v "callback") '()))))

(define higher-order
  (check-bridge spec (hash "Api" higher-order-code) (hash "Db" '(db))))
(check-equal? (length higher-order) 1)
(check-true (bridge-violation? (first higher-order)))
(check-equal? (bridge-violation-component (first higher-order)) "Api")
(check-equal? (bridge-violation-effect (first higher-order)) 'dynamic)

;; A member call on an unresolvable receiver is reported unverified, not a
;; violation: the method is known, the receiver type is not.
(define unknown-code
  (prog (fn "h" (uir-call (dot (v "obj") "m") '()))))

(define unk
  (check-bridge spec (hash "Api" unknown-code) (hash "Db" '(db))))
(check-equal? (length unk) 1)
(check-true (bridge-unverified? (first unk)))
(check-equal? (bridge-unverified-component (first unk)) "Api")

;; Components with no host code are skipped, not flagged.
(check-equal? (check-bridge spec (hash) (hash "Db" '(db))) '())

;; -- Phase 1.4: capability-interpretation values as effect rows ------
;; The interpretation table now accepts a full effect row (ops/budget/modality)
;; and the checker uses effect-subtype?, not flat name-set membership.

(define (ent name modality #:ops [ops '()] #:budget [budget #f])
  (uir-effect-entry (uir-symbol name) modality
                    (map uir-symbol ops) budget #f))
(define (row . entries) (uir-effect-row entries))

;; A capability restricted to `db.read` does NOT cover an inferred bare `db`
;; call (empty ops = "all ops", which is broader than `read`).
(define restricted-ops
  (check-bridge spec (hash "Api" clean-code)
                (hash "Db" (row (ent "db" 'may #:ops '("read"))))))
(check-equal? (length restricted-ops) 1)
(check-true (bridge-violation? (first restricted-ops)))
(check-equal? (bridge-violation-effect (first restricted-ops)) 'db)

;; A capability that OBLIGATES db ('must) covers an inferred 'may db use:
;; may ≤ must in the lattice.
(check-equal?
 (check-bridge spec (hash "Api" clean-code)
               (hash "Db" (row (ent "db" 'must))))
 '())

;; A budgeted capability does NOT cover an inferred unbounded use (budget #f).
(define restricted-budget
  (check-bridge spec (hash "Api" clean-code)
                (hash "Db" (row (ent "db" 'may #:budget 2)))))
(check-equal? (length restricted-budget) 1)
(check-true (bridge-violation? (first restricted-budget)))
(check-equal? (bridge-violation-effect (first restricted-budget)) 'db)

;; An entry-list interpretation value is accepted as a row too.
(check-equal?
 (check-bridge spec (hash "Api" clean-code)
               (hash "Db" (list (ent "db" 'may))))
 '())

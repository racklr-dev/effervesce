#lang racket

;; effervesce/harness/gate-test.rkt
;;
;; P5.4 — the effect gate: accept, isolate and rollback.
;;
;; The definition set is one component's UIR program, the contract is a
;; capability the component's `needs` covers, and the three decisions are
;; exercised end to end: a covered change lands in the live set, an uncovered
;; (or unresolvable) change is refused with the call that made it illegal, and
;; a change applied to the live image is restored by rollback.

(require rackunit
         racklr/uir
         "../load.rkt"
         "../check/bridge.rkt"
         "gate.rkt")

;; ── UIR builders ────────────────────────────────────────────────────

(define (v s) (uir-var (uir-symbol s)))
(define (dot base field) (uir-get base (uir-symbol field)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))
(define (sorted-effects d) (sort (decision-effects d) symbol<?))

;; ── fixtures ────────────────────────────────────────────────────────

(define spec
  (load-effervesce
   "system S {
      capability Db { query() -> Unit }
      given Db
      component Api needs Db { run() = query() }
    }"))

(define interpretations (hash "Db" '(db)))

;; The live definition of Api: one handler that reads the database.
(define clean-program
  (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))))

;; A change that adds a network call Api's contract does not cover.
(define leaky-program
  (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))
        (fn "fetcher" (uir-call (v "fetch") '()))))

;; A change whose call cannot be resolved to an effect at all.
(define unknown-program
  (prog (fn "handler" (uir-call (v "callback") '()))))

(define (live-gate)
  (gate-make spec interpretations #:definitions (hash "Api" clean-program)))

;; ── accept ──────────────────────────────────────────────────────────

(define (test-accept-commits-the-change)
  (define gate (live-gate))
  (define replacement
    (prog (fn "handler" (uir-call (dot (v "db") "select") '()))))
  (define d (gate-apply gate (proposal "Api" replacement)))
  (check-true (decision-accepted? d))
  (check-equal? (decision-kind d) 'accept)
  (check-equal? (decision-reason d) 'covered)
  (check-equal? (sorted-effects d) '(db))
  (check-equal? (decision-violations d) '())
  ;; The system modified its own definition set: the candidate is live.
  (check-equal? (gate-definition gate "Api") replacement)
  (check-equal? (gate-definition gate "Api") (decision-program d))
  ;; Nothing was refused, and the decision is in the log.
  (check-equal? (gate-quarantine gate) '())
  (check-equal? (gate-log gate) (list d))
  (check-true (regexp-match? #rx"accepted change to component Api.*db"
                              (first (decision-report d)))))

(define (test-accept-through-absorption)
  ;; The proposal arrives as serialized UIR (the journal/live-image shape);
  ;; the gate absorbs it before inferring and checking.
  (define gate (live-gate))
  (define d (gate-apply gate (proposal "Api" (uir->sexp clean-program))))
  (check-true (decision-accepted? d))
  (check-true (uir-block? (decision-program d)))
  (check-equal? (decision-program d) clean-program)
  (check-equal? (gate-definition gate "Api") clean-program))

(define (test-front-end-absorbs-the-source)
  ;; Absorption is an injectable seam: a front end turns proposed source into
  ;; UIR, and the gate infers and checks the absorbed program.
  (define absorbed '())
  (define (absorber component source)
    (set! absorbed (cons (list component source) absorbed))
    clean-program)
  (define gate (gate-make spec interpretations #:absorb absorber))
  (define d (gate-apply gate (proposal "Api" "def handler(): db.execute()\n")))
  (check-equal? (reverse absorbed) '(("Api" "def handler(): db.execute()\n")))
  (check-true (decision-accepted? d))
  (check-equal? (decision-program d) clean-program)
  (check-equal? (gate-definition gate "Api") clean-program))

;; ── isolate ─────────────────────────────────────────────────────────

(define (test-isolate-undeclared-component)
  ;; A component the description says nothing about has no contract that
  ;; covers its effects: the gate fails closed.
  (define gate (live-gate))
  (define d (gate-apply gate (proposal "Ghost" clean-program)))
  (check-true (decision-isolated? d))
  (check-equal? (decision-reason d) 'no-contract)
  (check-false (gate-definition gate "Ghost"))
  (check-equal? (gate-definition gate "Api") clean-program))

(define (test-isolate-uncovered-effect)
  (define gate (live-gate))
  (define d (gate-apply gate (proposal "Api" leaky-program)))
  (check-true (decision-isolated? d))
  (check-equal? (decision-kind d) 'isolate)
  (check-equal? (decision-reason d) 'uncovered)
  (check-equal? (sorted-effects d) '(db network))
  ;; The refusal names the offending effect and exactly which call made the
  ;; change illegal.
  (define violation (first (decision-violations d)))
  (check-equal? (bridge-violation-effect violation) 'network)
  (check-true (and (member "fetcher: fetch" (bridge-violation-sites violation))
                   #t))
  (check-true (regexp-match? #rx"isolated change to component Api"
                             (first (decision-report d))))
  (check-true (regexp-match? #rx"uncovered effect network via fetcher: fetch"
                             (second (decision-report d))))
  ;; The live definition set is untouched and the change is quarantined.
  (check-equal? (gate-definition gate "Api") clean-program)
  (check-equal? (length (gate-quarantine gate)) 1)
  (define refused (first (gate-quarantine gate)))
  (check-equal? (proposal-source (isolated-proposal refused)) leaky-program)
  (check-equal? (isolated-decision refused) d)
  (check-equal? (gate-log gate) (list d)))

(define (test-isolate-unresolvable-call)
  ;; `unknown` is a tracked obligation, not a pass: an inference the gate
  ;; cannot ground fails closed.
  (define gate (live-gate))
  (define d (gate-apply gate (proposal "Api" unknown-program)))
  (check-true (decision-isolated? d))
  (check-equal? (decision-reason d) 'unverified)
  (check-equal? (decision-unverified d) (list "handler: callback"))
  (check-equal? (gate-definition gate "Api") clean-program)
  (check-equal? (length (gate-quarantine gate)) 1))

(define (test-propose-only-checks)
  ;; The audit path decides without touching the live set or the quarantine.
  (define gate (live-gate))
  (define d (gate-propose gate (proposal "Api" leaky-program)))
  (check-true (decision-isolated? d))
  (check-equal? (gate-definition gate "Api") clean-program)
  (check-equal? (gate-quarantine gate) '())
  (check-equal? (gate-log gate) '()))

(define (test-coverage-comes-from-the-interpretation-table)
  ;; The same program is refused when the capability it needs no longer maps
  ;; to the effect it performs.
  (define gate (gate-make spec (hash "Db" '(io))
                          #:definitions (hash "Api" clean-program)))
  (define d (gate-apply gate (proposal "Api" clean-program)))
  (check-true (decision-isolated? d))
  (check-equal? (decision-reason d) 'uncovered)
  (check-equal? (bridge-violation-effect (first (decision-violations d))) 'db))

;; ── rollback ────────────────────────────────────────────────────────

(define (test-rollback-restores-the-live-image)
  (define gate (live-gate))
  (define d (gate-eval gate (proposal "Api" leaky-program)))
  (check-true (decision-rolled-back? d))
  (check-equal? (decision-kind d) 'rollback)
  (check-equal? (decision-reason d) 'uncovered)
  (check-equal? (bridge-violation-effect (first (decision-violations d))) 'network)
  ;; The failed redefinition was applied first, then undone: the live image
  ;; holds the previous definition again.
  (check-equal? (gate-definition gate "Api") clean-program)
  (check-equal? (decision-program d) clean-program)
  (check-equal? (gate-quarantine gate) '())
  (check-equal? (gate-log gate) (list d)))

(define (test-rollback-removes-a-new-component)
  ;; Rolling back a change that introduced a component removes it entirely.
  (define gate (gate-make spec interpretations))
  (define d (gate-eval gate (proposal "Api" leaky-program)))
  (check-true (decision-rolled-back? d))
  (check-false (gate-definition gate "Api"))
  (check-false (decision-program d)))

(define (test-eval-accepts-covered-change)
  (define gate (live-gate))
  (define replacement
    (prog (fn "handler" (uir-call (dot (v "db") "query") '()))))
  (define d (gate-eval gate (proposal "Api" replacement)))
  (check-true (decision-accepted? d))
  (check-equal? (gate-definition gate "Api") replacement))

(module+ main
  (test-accept-commits-the-change)
  (test-accept-through-absorption)
  (test-front-end-absorbs-the-source)
  (test-isolate-uncovered-effect)
  (test-isolate-unresolvable-call)
  (test-isolate-undeclared-component)
  (test-propose-only-checks)
  (test-coverage-comes-from-the-interpretation-table)
  (test-rollback-restores-the-live-image)
  (test-rollback-removes-a-new-component)
  (test-eval-accepts-covered-change)
  (displayln "All gate tests passed."))

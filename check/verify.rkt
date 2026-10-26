#lang racket

;; effervesce/check/verify.rkt
;;
;; System 2: the exact path. Runs all four checkers — type, policy, model,
;; bridge — and normalizes their heterogeneous issue structs into one
;; structured `finding` list with a severity. This is the escalation target
;; for any System 1 `ask` that returns `unknown`, and the source of truth for
;; "does this system meet its declared requirements".
;;
;; A finding is (checker severity kind message):
;;   checker  ∈ 'type | 'policy | 'model | 'bridge
;;   severity ∈ 'hard | 'note
;;   kind     a checker-specific symbol
;;
;; `verify-ok?` is true when there are no 'hard findings. Notes (e.g. an
;; invariant the model checker could not discharge) do not fail verification.

(require racklr/uir
         "type.rkt"
         "policy.rkt"
         "model.rkt"
         "bridge.rkt")

(provide
  verify verify-ok? verify-report
  finding finding? finding-checker finding-severity finding-kind finding-message)

(struct finding (checker severity kind message) #:transparent)

(define model-hard-kinds
  '(duplicate-state unknown-state-write unknown-state-reference invariant-violated))

(define (type-kind u)
  (cond
    [(unhandled-need? u) 'unhandled-need]
    [(unhandled-invocation? u) 'unhandled-invocation]
    [(missing-handler-op? u) 'missing-handler-op]
    [(missing-provided-op? u) 'missing-provided-op]
    [(unhandled-handler-invocation? u) 'unhandled-handler-invocation]
    [else 'type-issue]))

(define (type-findings issues)
  (map (λ (u m) (finding 'type 'hard (type-kind u) m))
       issues
       (check-type-report issues)))

(define (policy-findings issues)
  (map (λ (v m) (finding 'policy 'hard (policy-violation-kind v) m))
       issues
       (check-policy-report issues)))

(define (model-findings issues)
  (map (λ (i m)
         (define k (model-issue-kind i))
         (finding 'model (if (member k model-hard-kinds) 'hard 'note) k m))
       issues
       (check-model-report issues)))

(define (bridge-findings issues)
  (for/list ([i (in-list issues)])
    (cond
      [(bridge-violation? i)
       (define sites (bridge-violation-sites i))
       (finding 'bridge 'hard 'bridge-violation
                (format "component ~a performs uncovered effect ~a~a"
                        (bridge-violation-component i)
                        (bridge-violation-effect i)
                        (if (null? sites)
                            ""
                            (string-append " — via "
                                           (string-join
                                            (take sites (min 4 (length sites)))
                                            ", ")))))]
      [(bridge-unverified? i)
       (finding 'bridge 'note 'bridge-unverified
                (format "component ~a has unresolvable (higher-order) calls"
                        (bridge-unverified-component i)))]
      [else (finding 'bridge 'hard 'bridge-issue "unknown bridge finding")])))

;; Run every checker over a spec plus host code and the interpretation table.
(define (verify sys host-code interpretations)
  (append (type-findings (check-type sys))
          (policy-findings (check-policy sys))
          (model-findings (check-model sys))
          (bridge-findings (check-bridge sys host-code interpretations))))

(define (verify-ok? findings)
  (not (for/or ([f (in-list findings)])
         (eq? (finding-severity f) 'hard))))

(define (verify-report findings)
  (for/list ([f (in-list findings)])
    (format "[~a:~a] ~a" (finding-checker f) (finding-kind f) (finding-message f))))

(module+ main
  (displayln "effervesce/check/verify — System 2 verify surface loaded."))

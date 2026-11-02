#lang racket

;; effervesce/check/findings.rkt
;;
;; The normalized finding surface shared by check/verify.rkt and its
;; verification queries. A finding is (checker severity kind message):
;;   checker  ∈ 'type | 'policy | 'model | 'bridge
;;   severity ∈ 'hard | 'note
;;   kind     a checker-specific symbol
;;
;; The checker report functions are the single source of finding text: a
;; query builds the checker's issue struct and normalizes it here, so the
;; graph path and the struct path cannot drift in wording or severity.

(require racklr/uir
         "type.rkt"
         "policy.rkt"
         "model.rkt"
         "bridge.rkt")

(provide
  finding finding? finding-checker finding-severity finding-kind finding-message
  type-findings policy-findings model-findings bridge-findings
  type-finding policy-finding bridge-finding)

(struct finding (checker severity kind message) #:transparent)

(define model-hard-kinds
  '(duplicate-state unknown-state-write unknown-state-reference invariant-violated))

;; ── normalization ───────────────────────────────────────────────────
;;
;; The checker reports are the single source of finding text: a query builds
;; the checker's issue struct and normalizes it here, so the graph path and
;; the struct path cannot drift in wording or severity.

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

(define (type-finding u) (first (type-findings (list u))))
(define (policy-finding v) (first (policy-findings (list v))))
(define (bridge-finding i) (first (bridge-findings (list i))))

#lang racket

;; effervesce/harness/manifest-gate.rkt
;;
;; P5.3 — Loop as data: the modification gate.
;;
;; A proposed loop manifest is installed only when it passes the same gate any
;; other modification passes: `check/verify.rkt`'s type, policy, model, and
;; bridge checkers over the manifest's capability contract, with the manifest
;; program as the host code. The bridge inference is what turns a rewrite's
;; actual calls into an effect row; a rewrite that performs an effect its stage
;; does not need is refused, and the refusal names the effect and the call site
;; that produced it.
;;
;; The gate also checks the manifest's structural integrity first (every stage
;; is both a program function and a contract component, the driver exists, and
;; the manifest survives its own UIR round trip), so a rewrite cannot sneak an
;; unaccounted stage past the bridge checker by omitting its component.

(require racklr/uir
         racket/list
         "manifest.rkt"
         "serialize.rkt"
         "../check/verify.rkt")

(provide
 gate-manifest
 gate-verdict gate-verdict? gate-verdict-accepted? gate-verdict-manifest
 gate-verdict-findings gate-verdict-problems
 finding finding? finding-checker finding-severity finding-kind finding-message
 gate-refusals gate-refusal-messages)

(struct gate-verdict (accepted? manifest findings problems) #:transparent)

;; ── structural conformance ─────────────────────────────────────────

(define (serialization-round-trips? m)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (equal? (sexp->manifest (manifest->sexp m)) m)))

(define (structural-problems m)
  (define fns (manifest-function-names m))
  (define steps (manifest-step-names m))
  (define components (manifest-component-names m))
  (define driver (loop-manifest-driver m))
  (append
   (if (member driver fns)
       '()
       (list (format "driver function ~a is missing from the program" driver)))
   (if (member driver components)
       '()
       (list (format "driver component ~a is missing from the contract" driver)))
   (for/list ([step (in-list steps)] #:unless (member step components))
     (format "step function ~a has no contract component" step))
   (for/list ([c (in-list components)]
              #:unless (or (equal? c driver) (member c steps)))
     (format "contract component ~a has no program function" c))
   (if (serialization-round-trips? m)
       '()
       (list "manifest does not survive its own UIR serialization round trip"))))

;; ── host-code projection ───────────────────────────────────────────

;; The bridge checker reads one host-code program per component. Each stage is
;; checked through its own function body; the driver is checked through the
;; whole program so its inferred row is the union of the stages it sequences.
(define (manifest-host-code m)
  (define driver (loop-manifest-driver m))
  (define full-program (loop-manifest-program m))
  (for/fold ([host (hash)]) ([c (in-list (manifest-component-names m))])
    (cond
      [(equal? c driver) (hash-set host c full-program)]
      [(manifest-function m c)
       => (lambda (fn) (hash-set host c (uir-block (list fn))))]
      [else host])))

;; ── the gate ───────────────────────────────────────────────────────

(define (gate-manifest installed proposed)
  (unless (loop-manifest? proposed)
    (error 'gate-manifest "proposal is not a loop-manifest: ~e" proposed))
  (define problems (structural-problems proposed))
  (cond
    [(pair? problems) (gate-verdict #f installed '() problems)]
    [else
     (define findings
       (verify (loop-manifest-contract proposed)
               (manifest-host-code proposed)
               (manifest-interps-hash proposed)))
     (if (verify-ok? findings)
         (gate-verdict #t proposed findings '())
         (gate-verdict #f installed findings '()))]))

;; ── refusal helpers ────────────────────────────────────────────────

(define (gate-refusals verdict)
  (filter (lambda (f) (eq? (finding-severity f) 'hard))
          (gate-verdict-findings verdict)))

(define (gate-refusal-messages verdict)
  (append (gate-verdict-problems verdict)
          (map finding-message (gate-refusals verdict))))

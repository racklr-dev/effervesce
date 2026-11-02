#lang racket

;; effervesce/harness/manifest-test.rkt
;;
;; Definition-of-done tests for P5.3:
;;
;;   "The loop manifest is serializable UIR; a test rewrites the manifest
;;    through the gate and observes the new loop shape."
;;
;; One test case per behavior. The suite is run through rackunit/text-ui's
;; `run-tests` rather than by calling `check-*` directly: rackunit's default
;; check handler only records/prints FAILURE, so a bare failing `check-*` call
;; would still exit 0. `run-tests` returns the failure count and we turn a
;; non-zero count into a non-zero exit code.
;;
;; Run from effervesce/:
;;   PLTCOLLECTS="<repo>:" racket harness/manifest-test.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         racket/runtime-path
         racklr/uir
         "manifest.rkt"
         "serialize.rkt"
         "manifest-gate.rkt")

;; The checked-in example manifest, resolved relative to this source file so
;; the test does not depend on the process working directory.
(define-runtime-path checked-in-manifest "agent-loop.manifest.json")

;; ── expected data ──────────────────────────────────────────────────

;; The shape the base manifest's driver datum describes.
(define base-shape
  '(loop (seq (step "observe")
              (step "tick")
              (while (< round 3)
                     (seq (step "decide")
                          (step "act")
                          (step "verify")
                          (step "tick")
                          (set! "round" (+ round 1)))))))

;; The shape after the DoD rewrite: every stage moves inside the loop and the
;; bound becomes 2 -- a different datum, not different Racket code.
(define rewritten-shape
  '(loop (seq (while (< round 2)
                     (seq (step "observe")
                          (step "decide")
                          (step "act")
                          (step "verify")
                          (step "tick")
                          (set! "round" (+ round 1)))))))

;; Three rounds of decide/act/verify after the initial observe/tick pair.
(define base-trace
  '("observe" "tick"
    "decide" "act" "verify" "tick"
    "decide" "act" "verify" "tick"
    "decide" "act" "verify" "tick"))

;; Two rounds, with observe moved inside the loop.
(define rewritten-trace
  '("observe" "decide" "act" "verify" "tick"
    "observe" "decide" "act" "verify" "tick"))

;; ── helpers ────────────────────────────────────────────────────────

;; One stub handler per step, so a run observes dispatch order without
;; executing the effectful stage bodies.
(define (step-handlers m)
  (for/hash ([name (in-list (manifest-step-names m))])
    (values name (lambda args #t))))

(define (trace-names trace)
  (map step-event-name trace))

;; The DoD rewrite is a pure data edit of the driver function's body.
(define (rewrite-loop m)
  (manifest-with-driver-body
   m
   (uir-block
    (list
     (uir-while
      (call-of (uir-symbol "<")
               (list (loop-var-ref "round") (uir-number "2")))
      (uir-block
       (list
        (call-of (loop-var-ref "observe") '())
        (call-of (loop-var-ref "decide") '())
        (call-of (loop-var-ref "act") '())
        (call-of (loop-var-ref "verify") '())
        (call-of (loop-var-ref "tick") '())
        (loop-var-set! "round"
                       (call-of (uir-symbol "+")
                                (list (loop-var-ref "round") (uir-number "1"))))))
      (uir-null))))))

;; Run body on a throwaway JSON path, deleting it afterwards.
(define (with-temp-manifest-file body)
  (define path (make-temporary-file "harness-manifest-~a.json"))
  (dynamic-wind
    void
    (lambda () (body path))
    (lambda () (when (file-exists? path) (delete-file path)))))

;; ── 1. serialization ───────────────────────────────────────────────

(define (test-uir-serialization)
  (define m (base-loop-manifest))
  (define u (manifest->uir m))
  (check-true (uir? u) "manifest->uir returns a UIR node")
  (check-eq? (uir-tag u) 'record "the manifest is encoded as a UIR record")

  (check-equal? (sexp->manifest (manifest->sexp m)) m
                "manifest survives the versioned UIR sexp round trip")
  (check-equal? (json->manifest (manifest->json m)) m
                "manifest survives the versioned JSON string round trip"))

(define (test-file-serialization)
  (define m (base-loop-manifest))
  ;; A written JSON file reads back as the same manifest value.
  (with-temp-manifest-file
   (lambda (path)
     (write-manifest-file! path m)
     (check-true (file-exists? path) "write-manifest-file! creates the file")
     (check-equal? (read-manifest-file path) m
                   "written manifest file reads back equal")))
  ;; The checked-in example is exactly the base manifest.
  (check-true (file-exists? checked-in-manifest)
              "checked-in agent-loop.manifest.json exists")
  (check-equal? (read-manifest-file checked-in-manifest) m
                "checked-in agent-loop.manifest.json reads back as the base manifest"))

;; ── 2. version guard ───────────────────────────────────────────────

(define (test-version-guard)
  (define m (base-loop-manifest))
  (define v2 (struct-copy loop-manifest m [manifest-version 2]))
  (define u (manifest->uir v2))
  (check-true (uir? u) "an unsupported-version manifest still encodes as UIR")
  (check-exn exn:fail?
             (lambda () (uir->manifest u))
             "uir->manifest refuses an unsupported manifest schema version")
  (check-exn #rx"unsupported manifest schema version 2"
             (lambda () (uir->manifest u))
             "the version refusal names the unsupported version"))

;; ── 3. base loop shape ─────────────────────────────────────────────

(define (test-base-shape)
  (check-equal? (manifest-loop-shape (base-loop-manifest)) base-shape
                "the base manifest's loop shape is the exact expected datum"))

;; ── 4. the DoD rewrite, installed through the gate ─────────────────

(define (test-rewrite-through-gate)
  (define base (base-loop-manifest))
  (define rewritten (rewrite-loop base))

  (check-not-equal? (manifest-loop-shape rewritten) (manifest-loop-shape base)
                    "the rewrite changes the loop shape datum")

  (define verdict (gate-manifest base rewritten))
  (check-true (gate-verdict-accepted? verdict)
              (format "gate accepts the rewrite; refusals: ~e"
                      (gate-refusal-messages verdict)))
  (check-equal? (gate-verdict-manifest verdict) rewritten
                "the installed manifest is the rewrite")

  (define installed (gate-verdict-manifest verdict))
  (check-equal? (manifest-loop-shape installed) rewritten-shape
                "the installed loop shape is the exact rewritten datum")

  ;; Persistence: the new loop shape survives serialization of the accepted
  ;; manifest through a file and back.
  (with-temp-manifest-file
   (lambda (path)
     (write-manifest-file! path installed)
     (define reloaded (read-manifest-file path))
     (check-equal? reloaded installed
                   "the accepted manifest survives a file round trip")
     (check-equal? (manifest-loop-shape reloaded) rewritten-shape
                   "the rewritten loop shape survives persistence"))))

;; ── 5. the installed loop actually runs the new shape ──────────────

(define (test-rewritten-loop-runs)
  (define base (base-loop-manifest))
  (define rewritten (rewrite-loop base))

  (define-values (base-run base-final)
    (run-manifest base
                  #:handlers (step-handlers base)
                  #:state (hash "round" 0)))
  (check-equal? (trace-names base-run) base-trace
                "the base loop runs its base shape")
  (check-equal? (hash-ref base-final "round") 3
                "the base loop terminates with round = 3")

  (define-values (rewritten-run rewritten-final)
    (run-manifest rewritten
                  #:handlers (step-handlers rewritten)
                  #:state (hash "round" 0)))
  (check-equal? (trace-names rewritten-run) rewritten-trace
                "the executed loop follows the rewritten shape")
  (check-equal? (hash-ref rewritten-final "round") 2
                "the rewritten loop terminates with round = 2")
  (check-not-equal? (trace-names base-run) (trace-names rewritten-run)
                    "base and rewritten loops execute different traces"))

;; ── 6. gate refuses a step that performs an uncovered effect ───────

(define (test-gate-refuses-uncovered-effect)
  (define base (base-loop-manifest))
  ;; `decide` only needs Model->(llm); sending email is not covered.
  (define leaky
    (manifest-with-step-body
     base "decide"
     (call-of (op-of (loop-var-ref "smtplib") "sendmail")
              (list (uir-string "to@example.com")))))
  (define verdict (gate-manifest base leaky))

  (check-false (gate-verdict-accepted? verdict)
               "a step that performs an uncovered effect is refused")
  (check-equal? (gate-verdict-manifest verdict) base
                "a refused rewrite does not replace the installed manifest")

  (define messages (gate-refusal-messages verdict))
  (check-true (for/or ([message (in-list messages)])
                (and (string-contains? message "uncovered effect email")
                     (string-contains? message "decide: smtplib.sendmail")))
              (format "the refusal names the effect and its provenance site; got ~e"
                      messages))
  (check-equal? (manifest-loop-shape (gate-verdict-manifest verdict)) base-shape
                "the installed loop shape is still the base shape"))

;; ── 7. gate refuses a step with no contract component ──────────────

(define (test-gate-refuses-structural-rewrite)
  (define base (base-loop-manifest))
  (define extra-fn
    (fn-of "plan" (call-of (loop-var-ref "print") (list (uir-string "x")))))
  (define extra-program
    (uir-block
     (append (uir-block-stmts (loop-manifest-program base))
             (list extra-fn))))
  (define proposed (struct-copy loop-manifest base [program extra-program]))

  (define verdict (gate-manifest base proposed))
  (check-false (gate-verdict-accepted? verdict)
               "a program function with no contract component is refused")
  (define problems (gate-verdict-problems verdict))
  (check-true (pair? problems) "the structural refusal reports problems")
  (check-true (for/or ([problem (in-list problems)])
                (string-contains? problem "step function plan has no contract component"))
              (format "the structural refusal names the unaccounted step; got ~e"
                      problems))
  (check-equal? (gate-verdict-manifest verdict) base
                "the structural refusal leaves the installed manifest unchanged"))

;; ── 8. the untouched base manifest is accepted ─────────────────────

(define (test-gate-accepts-base)
  (define base (base-loop-manifest))
  (define verdict (gate-manifest base base))
  (check-true (gate-verdict-accepted? verdict)
              (format "the untouched base manifest is accepted; findings: ~e"
                      (map finding-message (gate-verdict-findings verdict))))
  (check-equal? (gate-verdict-manifest verdict) base
                "a no-op install keeps the base manifest")
  (check-equal? (gate-verdict-problems verdict) '()
                "an accepted manifest reports no structural problems"))

;; ── suite ──────────────────────────────────────────────────────────

(define harness-test-suite
  (test-suite
   "effervesce/harness/manifest-test.rkt"
   (test-case "UIR serialization round trips" (test-uir-serialization))
   (test-case "file serialization round trips" (test-file-serialization))
   (test-case "version guard" (test-version-guard))
   (test-case "base loop shape" (test-base-shape))
   (test-case "rewrite installed through gate" (test-rewrite-through-gate))
   (test-case "rewritten loop runs" (test-rewritten-loop-runs))
   (test-case "gate refuses uncovered effect" (test-gate-refuses-uncovered-effect))
   (test-case "gate refuses structural rewrite" (test-gate-refuses-structural-rewrite))
   (test-case "gate accepts base manifest" (test-gate-accepts-base))))

(module+ main
  (define failures (run-tests harness-test-suite))
  (if (zero? failures)
      (begin
        (displayln "All harness tests passed.")
        (exit 0))
      (exit 1)))

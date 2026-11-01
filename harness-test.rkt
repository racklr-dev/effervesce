#lang racket

;; Tests for the live image: the process, its eval surface, and the
;; redefinition-without-restart property.
;;
;; Two halves:
;;   - in-process tests of the harness state and the JSON protocol;
;;   - a smoke test that starts `harness/main.rkt` as a real long-lived
;;     subprocess, evaluates a definition, and queries the result. The pid
;;     stays the same across the redefinition, so the observation is proof
;;     the live system changed without a restart.
;;
;; Run from effervesce/:  racket harness-test.rkt

(require rackunit
         racket/file
         racket/os
         racket/port
         racket/runtime-path
         racket/system
         json
         (only-in "check.rkt" check-ok? check-report)
         "check/query.rkt"
         "check/verify.rkt"
         "harness/process.rkt"
         "harness/protocol.rkt")

;; ── fixtures ────────────────────────────────────────────────────────

(define base-text
  "system LiveBase {
     capability Store { add(text: String) -> Unit }
     given Store
     component App needs Store { addTodo(text) = add(text) }
   }")

;; A new definition: a second component, complete with its own capability.
(define worker-text
  "system WorkerLive {
     capability Log { write(msg: String) -> Unit }
     given Log
     component Worker needs Log { run() = write(\"hi\") }
   }")

;; The same definition, redefined to need a capability nothing provides.
(define worker-leaky-text
  "system WorkerLive {
     capability Log { write(msg: String) -> Unit }
     capability Notify { tell(msg: String) -> Unit }
     given Log
     component Worker needs Log, Notify { run() = write(\"ok\") }
   }")

;; ── helpers ─────────────────────────────────────────────────────────

(define (sorted-names v)
  (sort (for/list ([x (in-list v)])
          (cond
            [(string? x) x]
            [else (format "~a" x)]))
        string<?))

(define (request h req)
  (handle-line h (jsexpr->string req)))

(define (ask-capabilities h name)
  (define-values (resp _h _shutdown?)
    (request h (hash 'op "ask" 'question (list "capabilities-of" name))))
  (if (hash-ref resp 'ok #f)
      (sorted-names (hash-ref resp 'value '()))
      (error 'harness-test "ask failed: ~a" (hash-ref resp 'error "?"))))

;; ── in-process state tests ──────────────────────────────────────────

(define (test-make-harness-loads-base)
  (define h (make-harness "base" base-text))
  (check-equal? (harness-generation h) 0)
  (check-equal? (harness-definition-names h) '("base"))
  (check-equal? (harness-pid h) (getpid))
  (check-equal? (ask-capabilities h "App") '("Store")))

(define (test-eval-adds-a-definition)
  (define h (make-harness "base" base-text))
  (define h* (harness-eval h "worker" worker-text))
  (check-equal? (harness-generation h*) 1)
  (check-equal? (harness-definition-names h*) '("base" "worker"))
  (check-equal? (ask-capabilities h* "Worker") '("Log"))
  ;; The live system now carries both definitions' declarations.
  (define counts (decl-kind-counts (harness-system h*)))
  (check-equal? (hash-ref counts 'capabilities) 2)
  (check-equal? (hash-ref counts 'components) 2)
  ;; The previous harness is untouched: transitions are functional.
  (check-equal? (harness-generation h) 0)
  (check-equal? (harness-definition-names h) '("base")))

(define (test-redefinition-is-observable)
  (define h (make-harness "base" base-text))
  (define h1 (harness-eval h "worker" worker-text))
  (define h2 (harness-eval h1 "worker" worker-leaky-text))
  (check-equal? (harness-generation h2) 2)
  (check-equal? (harness-definition-names h2) '("base" "worker"))
  ;; Same definition name, same process, different live behavior.
  (check-equal? (harness-pid h2) (harness-pid h1))
  (check-equal? (ask-capabilities h1 "Worker") '("Log"))
  (check-equal? (ask-capabilities h2 "Worker") '("Log" "Notify"))
  (check-true (check-ok? (harness-check h1)))
  (check-false (check-ok? (harness-check h2)))
  (check-true (verify-ok? (harness-verify h1)))
  (check-false (verify-ok? (harness-verify h2)))
  (check-true (for/or ([line (in-list (check-report (harness-check h2)))])
                (regexp-match? #rx"Notify" line))))

(define (test-invalid-definition-is-atomic)
  (define h (make-harness "base" base-text))
  (define h1 (harness-eval h "worker" worker-text))
  (define raised?
    (with-handlers ([exn:fail? (λ (e) #t)])
      (harness-eval h1 "broken" "system Broken {")
      #f))
  (check-true raised?)
  ;; The rejected definition never reached the live system.
  (check-equal? (harness-generation h1) 1)
  (check-equal? (harness-definition-names h1) '("base" "worker"))
  (check-equal? (ask-capabilities h1 "Worker") '("Log"))
  ;; The protocol reports the rejection and keeps the process usable.
  (define-values (resp h2 shutdown?)
    (request h1 (hash 'op "eval" 'name "broken" 'text "system Broken {")))
  (check-false (hash-ref resp 'ok #f))
  (check-equal? (hash-ref resp 'generation) 1)
  (check-equal? (harness-generation h2) 1)
  (define-values (resp2 h2b shutdown2?)
    (request h2 (hash 'op "ask" 'question (list "capabilities-of" "Worker"))))
  (check-true (hash-ref resp2 'ok #f))
  (check-equal? (sorted-names (hash-ref resp2 'value '())) '("Log")))

(define (test-remove-drops-a-definition)
  (define h (make-harness "base" base-text))
  (define h1 (harness-eval h "worker" worker-text))
  (define h2 (harness-remove h1 "worker"))
  (check-equal? (harness-generation h2) 2)
  (check-equal? (harness-definition-names h2) '("base"))
  (check-equal? (ask-capabilities h2 "Worker") '())
  (check-true (check-ok? (harness-check h2))))

(define (test-query-vocabulary)
  (define h (make-harness "base" base-text))
  (define idx (harness-index h))
  (check-true (answer-value (ask idx '(reachable? ("Store")))))
  (check-false (answer-value (ask (harness-index (harness-eval h "worker" worker-text))
                                  '(reachable? ("Notify"))))))

(define (test-bad-request-does-not-end-the-process)
  (define h (make-harness "base" base-text))
  (define-values (resp h* shutdown?) (handle-line h "not json"))
  (check-false (hash-ref resp 'ok #f))
  (check-false shutdown?)
  (check-equal? (harness-generation h*) 0)
  (define-values (resp2 h2 _) (handle-line h* "{\"op\":\"stat\"}"))
  (check-true (hash-ref resp2 'ok #f))
  (check-equal? (hash-ref resp2 'pid) (harness-pid h2)))

;; ── long-lived subprocess smoke test ────────────────────────────────

(define-runtime-path harness-main "harness/main.rkt")
(define-runtime-path base-spec "harness/examples/live-base.fizz")

;; The child must resolve racklr/ and effervesce/ from this checkout, not
;; from whatever the user-level package links happen to point at.
(define (child-environment)
  (define here (simplify-path (build-path (path->complete-path harness-main) 'up 'up)))
  (define env (environment-variables-copy (current-environment-variables)))
  (define existing (or (getenv "PLTCOLLECTS") ""))
  (environment-variables-set!
   env #"PLTCOLLECTS"
   (string->bytes/utf-8
    (string-append (path->string here)
                   (if (string=? existing "") ":" (string-append ":" existing)))))
  env)

(define (read-response in)
  (define line (sync/timeout 60 (read-line-evt in 'any)))
  (cond
    [(eq? line #f) (error 'harness-test "timed out waiting for a response line")]
    [(eof-object? line) (error 'harness-test "live process closed its output")]
    [else (string->jsexpr line)]))

(define (send-request! out req)
  (displayln (jsexpr->string req) out)
  (flush-output out))

(define (stop-live-process child-out child-in control)
  (with-handlers ([exn:fail? void])
    (if (eq? (control 'status) 'running)
        (control 'kill)
        (void)))
  (with-handlers ([exn:fail? void]) (close-input-port child-out))
  (with-handlers ([exn:fail? void]) (close-output-port child-in)))

;; Start the real process and answer the ready line.
(define (start-live-process)
  (define racket-bin (or (find-executable-path "racket")
                         (find-system-path 'exec-file)))
  (define proc
    (parameterize ([current-environment-variables (child-environment)])
      (apply process* (path->string racket-bin)
             (list (path->string harness-main) "--spec" (path->string base-spec)))))
  (define child-out (list-ref proc 0))
  (define child-in (list-ref proc 1))
  (define child-pid (list-ref proc 2))
  (define control (list-ref proc 4))
  (define ready (read-response child-out))
  (values child-out child-in child-pid control ready))

(define (test-live-process-eval-then-query)
  (define-values (child-out child-in child-pid control ready)
    (start-live-process))
  (dynamic-wind
   void
   (λ ()
     ;; The process is up and reports itself on the protocol.
     (check-true (hash-ref ready 'ready #f))
     (check-equal? (hash-ref ready 'pid) child-pid)
     (check-equal? (hash-ref ready 'generation) 0)
     (check-equal? (hash-ref ready 'definitions) '("base"))

     (define pid child-pid)

     (define (ask name)
       (send-request! child-in (hash 'op "ask"
                                     'question (list "capabilities-of" name)))
       (define resp (read-response child-out))
       (check-equal? (hash-ref resp 'pid) pid)
       (sorted-names (hash-ref resp 'value '())))

     ;; The loaded system before any modification.
     (check-equal? (ask "App") '("Store"))

     ;; Accept a definition into the running process.
     (send-request! child-in (hash 'op "eval" 'name "worker" 'text worker-text))
     (define add-resp (read-response child-out))
     (check-true (hash-ref add-resp 'ok #f))
     (check-false (hash-ref add-resp 'redefined #f))
     (check-equal? (hash-ref add-resp 'generation) 1)
     (check-equal? (hash-ref add-resp 'pid) pid)
     (check-equal? (hash-ref add-resp 'definitions) '("base" "worker"))
     (check-equal? (ask "Worker") '("Log"))

     ;; Redefine the same name. The pid and the live image stay the same;
     ;; the queried behavior changes, so the redefinition is observable
     ;; without a restart.
     (send-request! child-in (hash 'op "eval" 'name "worker" 'text worker-leaky-text))
     (define redef-resp (read-response child-out))
     (check-true (hash-ref redef-resp 'ok #f))
     (check-true (hash-ref redef-resp 'redefined #f))
     (check-equal? (hash-ref redef-resp 'generation) 2)
     (check-equal? (hash-ref redef-resp 'pid) pid)
     (check-equal? (ask "Worker") '("Log" "Notify"))

     ;; The checker observes the same changed live system.
     (send-request! child-in (hash 'op "check"))
     (define check-resp (read-response child-out))
     (check-equal? (hash-ref check-resp 'pid) pid)
     (check-false (hash-ref check-resp 'check-ok #f))
     (check-true (for/or ([line (in-list (hash-ref check-resp 'report '()))])
                   (regexp-match? #rx"Notify" line)))

     ;; A rejected definition leaves the live system exactly where it was.
     (send-request! child-in (hash 'op "eval" 'name "worker" 'text "system Broken {"))
     (define bad-resp (read-response child-out))
     (check-false (hash-ref bad-resp 'ok #f))
     (check-equal? (hash-ref bad-resp 'generation) 2)
     (send-request! child-in (hash 'op "stat"))
     (define stat-resp (read-response child-out))
     (check-equal? (hash-ref stat-resp 'pid) pid)
     (check-equal? (hash-ref stat-resp 'generation) 2)
     (check-equal? (hash-ref stat-resp 'definitions) '("base" "worker"))
     (check-equal? (ask "Worker") '("Log" "Notify"))

     ;; Shutdown ends the same process cleanly.
     (send-request! child-in (hash 'op "shutdown"))
     (define bye (read-response child-out))
     (check-true (hash-ref bye 'ok #f))
     (check-equal? (hash-ref bye 'event) "shutdown")
     (define end (sync/timeout 30 (read-line-evt child-out 'any)))
     (check-true (eof-object? end)))
   (λ () (stop-live-process child-out child-in control))))

(module+ main
  (test-make-harness-loads-base)
  (test-eval-adds-a-definition)
  (test-redefinition-is-observable)
  (test-invalid-definition-is-atomic)
  (test-remove-drops-a-definition)
  (test-query-vocabulary)
  (test-bad-request-does-not-end-the-process)
  (test-live-process-eval-then-query)
  (displayln "All harness tests passed."))

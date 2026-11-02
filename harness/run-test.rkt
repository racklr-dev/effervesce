#lang racket

;; effervesce/harness/run-test.rkt
;;
;; Tests for P5.3's runner: the UIR interpreter executes the driver function
;; from manifest data and records the ordered step trace. Every assertion runs
;; the interpreter; nothing is skipped.

(require rackunit
         rackunit/text-ui
         racklr/uir
         "run.rkt")

;; ── UIR shorthands ─────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (sym s)))
(define (num n) (uir-number (number->string n)))
(define (setv name value) (uir-set! (sym name) value))
(define (call name . args) (uir-call (v name) args))
(define (op name . args) (uir-call (sym name) args))
(define (get-chain base field) (uir-get (v base) (sym field)))
(define (op-call base field . args) (uir-call (get-chain base field) args))
(define (block . stmts) (uir-block stmts))
(define (fn name body) (uir-fn (sym name) '() body #f #f))
(define (program . fns) (uir-block fns))
(define (trace-names trace) (map step-event-name trace))
(define (trace-values trace) (map step-event-value trace))

;; ── the loop, as data ──────────────────────────────────────────────

;; Step functions; their bodies are the effectful work a real harness would
;; perform. Handlers shadow them in the trace tests, so the bodies never run.
(define (loop-fns)
  (list (fn "observe"
            (op-call "requests" "get" (uir-string "https://api.github.com/board")))
        (fn "decide" (call "inner-decide"))
        (fn "act" (op-call "open" "write" (uir-string "work.log")))
        (fn "verify" (op-call "subprocess" "run" (uir-string "racket tests")))
        (fn "tick" (op-call "time" "time"))))

;; The driver body is the loop. `bound-expr` is the only datum that differs
;; between the "bound 3" and "bound 2" runs below.
(define (driver-body bound-expr)
  (block
   (call "observe")
   (call "tick")
   (uir-while
    (op "<" (v "round") bound-expr)
    (block
     (call "decide")
     (call "act")
     (call "verify")
     (call "tick")
     (setv "round" (op "+" (v "round") (num 1))))
    (uir-null))))

(define (loop-program bound-expr)
  (apply program (append (loop-fns)
                         (list (fn "driver" (driver-body bound-expr))))))

(define (loop-handlers)
  (hash "observe" (lambda () 'observed)
        "decide" (lambda () 'decided)
        "act" (lambda () 'acted)
        "verify" (lambda () 'verified)
        "tick" (lambda () 'ticked)))

(define expected-round-trace
  '("observe" "tick"
    "decide" "act" "verify" "tick"
    "decide" "act" "verify" "tick"
    "decide" "act" "verify" "tick"))

(define expected-round-values
  '(observed ticked
    decided acted verified ticked
    decided acted verified ticked
    decided acted verified ticked))

;; ── tests ──────────────────────────────────────────────────────────

(define (test-exact-trace-order)
  (define-values (trace state)
    (run-loop (loop-program (v "bound")) "driver"
              #:handlers (loop-handlers)
              #:state (hash "round" 0 "bound" 3)))
  (check-true (andmap step-event? trace))
  (check-equal? (trace-names trace) expected-round-trace)
  (check-equal? (trace-values trace) expected-round-values)
  ;; The counter is loop state, mutated through the seeded environment.
  (check-equal? (hash-ref state "round") 3))

(define (test-loop-is-data)
  ;; Same functions, same handlers; only the driver-body datum changes.
  (define-values (trace3 state3)
    (run-loop (loop-program (num 3)) "driver"
              #:handlers (loop-handlers)
              #:state (hash "round" 0)))
  (define-values (trace2 state2)
    (run-loop (loop-program (num 2)) "driver"
              #:handlers (loop-handlers)
              #:state (hash "round" 0)))
  (check-equal? (trace-names trace3) expected-round-trace)
  (check-equal? (trace-names trace2)
                '("observe" "tick"
                  "decide" "act" "verify" "tick"
                  "decide" "act" "verify" "tick"))
  (check-not-equal? (trace-names trace3) (trace-names trace2))
  (check-equal? (hash-ref state3 "round") 3)
  (check-equal? (hash-ref state2 "round") 2))

(define (test-handler-precedence)
  ;; The program has a `decide` function whose body calls `inner-decide`; the
  ;; handler for `decide` must win, so that body is never interpreted.
  (define p (program (fn "decide" (call "inner-decide"))
                     (fn "driver" (block (call "decide")))))
  (define-values (trace _state)
    (run-loop p "driver"
              #:handlers (hash "decide" (lambda () 'handled)
                               "inner-decide" (lambda () 'inner))))
  (check-equal? (trace-names trace) '("decide"))
  (check-equal? (trace-values trace) (list 'handled))
  ;; Without a handler for `decide`, the program function is interpreted.
  (define-values (trace* _state*)
    (run-loop p "driver" #:handlers (hash "inner-decide" (lambda () 'inner))))
  (check-equal? (trace-names trace*) '("inner-decide"))
  (check-equal? (trace-values trace*) (list 'inner)))

(define (test-if-branches)
  (define p (program
             (fn "driver"
                 (block (uir-if (call "decide") (call "act") (call "skip"))))))
  (define-values (trace-yes _yes)
    (run-loop p "driver"
              #:handlers (hash "decide" (lambda () #t)
                               "act" (lambda () 'acted)
                               "skip" (lambda () 'skipped))))
  (check-equal? (trace-names trace-yes) '("decide" "act"))
  (check-equal? (trace-values trace-yes) (list #t 'acted))
  (define-values (trace-no _no)
    (run-loop p "driver"
              #:handlers (hash "decide" (lambda () #f)
                               "act" (lambda () 'acted)
                               "skip" (lambda () 'skipped))))
  (check-equal? (trace-names trace-no) '("decide" "skip"))
  ;; uir-null evaluates to #f, so an explicit null test takes the else branch.
  (define p-null
    (program (fn "driver"
                 (block (uir-if (uir-null) (call "act") (call "skip"))))))
  (define-values (trace-null _null)
    (run-loop p-null "driver"
              #:handlers (hash "act" (lambda () 'acted)
                               "skip" (lambda () 'skipped))))
  (check-equal? (trace-names trace-null) '("skip")))

(define (test-let-scoping-and-mutation)
  (define p
    (program
     (fn "driver"
         (block
          (uir-let (sym "local") (num 7)
                   (block
                    (setv "total" (op "+" (v "total") (v "local")))
                    (setv "local" (op "+" (v "local") (num 1)))
                    (setv "total" (op "+" (v "total") (v "local")))))))))
  (define-values (trace state)
    (run-loop p "driver" #:state (hash "total" 0)))
  (check-equal? (trace-names trace) '())
  (check-equal? (hash-ref state "total") 15)
  ;; The let binding does not escape its body: it is restored, not left over.
  (check-false (hash-has-key? state "local"))
  ;; The let value is evaluated in the outer scope before the name is bound.
  (define p-shadow
    (program
     (fn "driver"
         (uir-let (sym "local")
                  (op "+" (v "local") (num 1))
                  (setv "total" (v "local"))))))
  (define-values (_trace2 state2)
    (run-loop p-shadow "driver" #:state (hash "local" 10 "total" 0)))
  (check-equal? (hash-ref state2 "total") 11)
  (check-equal? (hash-ref state2 "local") 10))

(define (test-handler-receives-evaluated-args)
  (define p
    (program
     (fn "driver"
         (block (call "record" (num 5) (op "+" (num 1) (num 2)))))))
  (define-values (trace _)
    (run-loop p "driver" #:handlers (hash "record" (lambda (a b) (+ a b)))))
  (check-equal? (trace-names trace) '("record"))
  (check-equal? (trace-values trace) (list 8)))

(define (test-opaque-get-chain)
  ;; A get chain with no handler is an opaque stage: it still traces, with a
  ;; #f result, and does not need its receiver to be loop state.
  (define p (program (fn "driver" (block (op-call "time" "time")))))
  (define-values (trace _)
    (run-loop p "driver"))
  (check-equal? (trace-names trace) '("time.time"))
  (check-equal? (trace-values trace) (list #f))
  ;; A handler keyed by the dotted name supplies the opaque stage's value.
  (define-values (trace* _star)
    (run-loop p "driver" #:handlers (hash "time.time" (lambda () 1234))))
  (check-equal? (trace-names trace*) '("time.time"))
  (check-equal? (trace-values trace*) (list 1234)))

(define (test-return-unwinds)
  (define p
    (program
     (fn "one"
         (block
          (uir-return (num 7))
          (call "after-one")))
     (fn "driver"
         (block
          (call "one")
          (call "observe")
          (uir-return (num 42))
          (call "after-driver")))))
  (define-values (trace _state)
    (run-loop p "driver"
              #:handlers (hash "observe" (lambda () 'observed)
                               "after-one" (lambda () 'nope)
                               "after-driver" (lambda () 'nope))))
  ;; A return in `one` stops only `one`; the driver's return stops the driver.
  (check-equal? (trace-names trace) '("observe"))
  (check-equal? (trace-values trace) (list 'observed))
  ;; A return from inside a while body interrupts the loop, keeping the trace
  ;; recorded up to the interruption.
  (define p-loop
    (program
     (fn "driver"
         (uir-while (uir-bool #t)
                    (block
                     (call "observe")
                     (uir-return (num 1))
                     (call "after-observe"))
                    (uir-null)))))
  (define-values (trace-loop _)
    (run-loop p-loop "driver"
              #:handlers (hash "observe" (lambda () 'observed)
                               "after-observe" (lambda () 'nope))))
  (check-equal? (trace-names trace-loop) '("observe")))

(define (test-literals)
  (define p
    (program
     (fn "driver"
         (block
          (setv "n" (num 42))
          (setv "b" (uir-bool #t))
          (setv "s" (uir-string "hello"))
          (setv "z" (uir-null))
          ;; A bare uir-symbol in value position is a variable reference.
          (setv "from-symbol" (sym "n"))))))
  (define-values (trace state)
    (run-loop p "driver"))
  (check-equal? (trace-names trace) '())
  (check-equal? (hash-ref state "n") 42)
  (check-equal? (hash-ref state "b") #t)
  (check-equal? (hash-ref state "s") "hello")
  (check-false (hash-ref state "z" #t))
  (check-equal? (hash-ref state "from-symbol") 42))

(define (test-primitive-operators)
  (define p
    (program
     (fn "driver"
         (block
          (setv "sum" (op "+" (num 1) (num 2) (num 3)))
          (setv "diff" (op "-" (num 10) (num 4)))
          (setv "prod" (op "*" (num 6) (num 7)))
          (setv "quotient" (op "/" (num 8) (num 2)))
          (setv "eq" (op "=" (num 2) (num 2)))
          (setv "ne" (op "!=" (num 2) (num 3)))
          (setv "lt" (op "<" (num 1) (num 2)))
          (setv "gt" (op ">" (num 2) (num 1)))
          (setv "leq" (op "<=" (num 1) (num 1)))
          (setv "geq" (op ">=" (num 2) (num 1)))
          (setv "negated" (op "not" (uir-bool #f)))
          (setv "anded" (op "and" (uir-bool #t) (num 5)))
          (setv "ored" (op "or" (uir-bool #f) (num 6)))))))
  (define-values (_trace state) (run-loop p "driver"))
  (check-equal? (hash-ref state "sum") 6)
  (check-equal? (hash-ref state "diff") 6)
  (check-equal? (hash-ref state "prod") 42)
  (check-equal? (hash-ref state "quotient") 4)
  (check-equal? (hash-ref state "eq") #t)
  (check-equal? (hash-ref state "ne") #t)
  (check-equal? (hash-ref state "lt") #t)
  (check-equal? (hash-ref state "gt") #t)
  (check-equal? (hash-ref state "leq") #t)
  (check-equal? (hash-ref state "geq") #t)
  (check-equal? (hash-ref state "negated") #t)
  (check-equal? (hash-ref state "anded") 5)
  (check-equal? (hash-ref state "ored") 6))

(define (test-missing-driver-errors)
  (check-exn #rx"driver function .* not found"
             (lambda ()
               (run-loop (program (fn "driver" (block))) "missing"))))

(define (test-malformed-program-errors)
  (check-exn #rx"must be named uir-fns"
             (lambda () (run-loop (uir-block (list (num 1))) "driver")))
  (check-exn #rx"unnamed function"
             (lambda ()
               (run-loop (uir-block (list (uir-fn #f '() (uir-null) #f #f)))
                         "driver"))))

(define (test-unknown-node-errors)
  (check-exn #rx"unsupported UIR node tag: for-each"
             (lambda ()
               (run-loop (program
                          (fn "driver"
                              (uir-for-each (sym "x") (uir-list '())
                                            (uir-null) (uir-null))))
                         "driver"))))

(define (test-runaway-loop-errors)
  (check-exn #rx"step limit exceeded|iterations"
             (lambda ()
               (run-loop (program
                          (fn "driver"
                              (uir-while (uir-bool #t) (uir-null) (uir-null))))
                         "driver"))))

;; The suite is run through `run-tests` rather than by direct calls: rackunit's
;; default check handler only prints FAILURE, so direct calls would exit 0 on a
;; failing assertion. `run-tests` returns the failure count and we turn a
;; non-zero count into a non-zero exit.
(define (runner-test-suite)
  (test-suite
   "effervesce/harness/run.rkt"
   (test-case "exact trace order" (test-exact-trace-order))
   (test-case "loop is data" (test-loop-is-data))
   (test-case "handler precedence" (test-handler-precedence))
   (test-case "if branches" (test-if-branches))
   (test-case "let scoping and mutation" (test-let-scoping-and-mutation))
   (test-case "handler receives evaluated args" (test-handler-receives-evaluated-args))
   (test-case "opaque get chain" (test-opaque-get-chain))
   (test-case "return unwinds" (test-return-unwinds))
   (test-case "literals" (test-literals))
   (test-case "primitive operators" (test-primitive-operators))
   (test-case "missing driver errors" (test-missing-driver-errors))
   (test-case "malformed program errors" (test-malformed-program-errors))
   (test-case "unknown node errors" (test-unknown-node-errors))
   (test-case "runaway loop errors" (test-runaway-loop-errors))))

(module+ main
  (define failures (run-tests (runner-test-suite)))
  (if (zero? failures)
      (displayln "All run tests passed.")
      (exit 1)))

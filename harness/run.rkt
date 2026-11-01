#lang racket

;; effervesce/harness/run.rkt
;;
;; P5.3 — Loop as data: the runner.
;;
;; `run-loop` executes the driver function that the manifest names, reading the
;; body out of the UIR rather than from Racket control flow. Step calls are
;; dispatched to stub handlers that record an ordered `step-event` trace; the
;; runner returns (values trace final-state). A change to the driver datum is
;; therefore a change to the executed loop, with no Racket edit.
;;
;; This module depends only on `racklr/uir`: nothing here knows what a manifest
;; is, so the interpreter cannot smuggle loop structure back into Racket.

(require racklr/uir)

(provide step-event step-event? step-event-name step-event-value run-loop)

;; ── trace events ───────────────────────────────────────────────────

(struct step-event (name value) #:transparent)

;; Runaway guards. A well-formed manifest loop finishes well inside these; a
;; trip means the data describes a non-terminating loop.
(define STEP-LIMIT 10000)
(define LOOP-LIMIT 10000)

;; Shared mutable interpreter state:
;;   env      string -> Racket value (seeded from #:state, mutated by uir-set!)
;;   events   reversed step-events, reversed once by run-loop
;;   nodes    interpreted-node count, for the runaway guard
;;   handlers string -> procedure, the stub step handlers
;;   fns      manifest function name -> uir-fn
(struct runtime (env events nodes handlers fns) #:transparent)

;; ── node names ─────────────────────────────────────────────────────

;; Plain string name of a symbol/var node, otherwise #f.
(define (plain-name node)
  (cond
    [(uir-symbol? node) (uir-symbol-name node)]
    [(uir-var? node) (plain-name (uir-var-name node))]
    [else #f]))

;; Dotted spelling of a receiver chain, e.g.
;; (get (var "time") (symbol "time")) -> "time.time"; #f when a base or field
;; is not a name (e.g. a call result).
(define (dotted-name node)
  (cond
    [(or (uir-symbol? node) (uir-var? node)) (plain-name node)]
    [(uir-get? node)
     (define base (dotted-name (uir-get-base node)))
     (define field (plain-name (uir-get-field node)))
     (and base field (string-append base "." field))]
    [else #f]))

(define (truthy? value) (if (eq? value #f) #f #t))

(define (safe-tag node)
  (if (uir? node) (uir-tag node) node))

;; ── guards and trace recording ─────────────────────────────────────

(define (require! ok? message . args)
  (if ok?
      (void)
      (apply error 'run-loop message args)))

(define (tick! rt node)
  (define n (add1 (unbox (runtime-nodes rt))))
  (set-box! (runtime-nodes rt) n)
  (if (> n STEP-LIMIT)
      (error 'run-loop
             "step limit exceeded (~a interpreted nodes); the driver likely does not terminate (last node tag: ~s)"
             STEP-LIMIT (safe-tag node))
      (void)))

(define (record! rt name value)
  (set-box! (runtime-events rt)
            (cons (step-event name value) (unbox (runtime-events rt))))
  value)

;; ── environment ────────────────────────────────────────────────────

(define (env-ref rt name node)
  (define env (runtime-env rt))
  (if (hash-has-key? env name)
      (hash-ref env name)
      (error 'run-loop "unbound variable ~s while interpreting ~s"
             name (safe-tag node))))

(define (env-set! rt name value)
  (hash-set! (runtime-env rt) name value)
  value)

;; Bind each (name . value) for the dynamic extent of thunk, restoring the
;; previous environment afterwards. dynamic-wind restores even when a
;; uir-return escapes the thunk.
(define (call-with-bindings rt binds thunk)
  (define env (runtime-env rt))
  (define saved
    (for/list ([b (in-list binds)])
      (define name (car b))
      (list name (hash-has-key? env name) (hash-ref env name #f))))
  (dynamic-wind
   (lambda () (for ([b (in-list binds)]) (hash-set! env (car b) (cdr b))))
   thunk
   (lambda ()
     (for ([s (in-list saved)])
       (if (second s)
           (hash-set! env (first s) (third s))
           (hash-remove! env (first s)))))))

(define (binding-name node what)
  (or (plain-name node)
      (error 'run-loop "~a has no plain name: ~s" what (safe-tag node))))

;; ── manifest functions ─────────────────────────────────────────────

;; A manifest statement must be a named uir-fn; anything else is a malformed
;; program and must fail loudly rather than be skipped.
(define (declared-fn stmt)
  (cond
    [(not (uir-fn? stmt))
     (error 'run-loop
            "manifest program statements must be named uir-fns; found ~s"
            (safe-tag stmt))]
    [(not (plain-name (uir-fn-name stmt)))
     (error 'run-loop "manifest program contains an unnamed function: ~s" stmt)]
    [else stmt]))

(define (build-fn-table stmts)
  (for/fold ([table (hash)]) ([stmt (in-list stmts)])
    (define fn (declared-fn stmt))
    (define name (plain-name (uir-fn-name fn)))
    (if (hash-has-key? table name)
        (error 'run-loop "duplicate function name ~s in manifest program" name)
        (hash-set table name fn))))

(define (driver-arguments rt fn)
  (for/list ([p (in-list (uir-fn-params fn))])
    (define name (binding-name p 'driver-parameter))
    (if (hash-has-key? (runtime-env rt) name)
        (hash-ref (runtime-env rt) name)
        (error 'run-loop
               "driver parameter ~a is not seeded in #:state; seed it with (hash ~s <value>)"
               name name))))

(define current-return (make-parameter #f))

;; Apply a named manifest function. uir-return unwinds through the escape
;; continuation installed for this call only, so a return in one function does
;; not escape its caller.
(define (apply-fn rt fn args)
  (define params (uir-fn-params fn))
  (define name (or (plain-name (uir-fn-name fn)) "<anonymous>"))
  (if (not (= (length params) (length args)))
      (error 'run-loop "function ~a expects ~a argument(s), got ~a"
             name (length params) (length args))
      (let ([binds (for/list ([p (in-list params)] [a (in-list args)])
                     (cons (binding-name p 'function-parameter) a))])
        (call-with-bindings rt binds
          (lambda ()
            (let/ec escape
              (parameterize ([current-return escape])
                (eval-node rt (uir-fn-body fn)))))))))

;; ── evaluator ──────────────────────────────────────────────────────

(define (literal-number node)
  (define raw (uir-number-value node))
  (cond
    [(number? raw) raw]
    [(string? raw) (or (string->number raw)
                       (error 'run-loop "invalid numeric literal: ~s" raw))]
    [else (error 'run-loop "uir-number holds a non-number value: ~s" raw)]))

(define (eval-node rt node)
  (tick! rt node)
  (cond
    [(uir-null? node) #f]
    [(uir-bool? node) (uir-bool-value node)]
    [(uir-number? node) (literal-number node)]
    [(uir-string? node) (uir-string-value node)]
    [(uir-var? node) (env-ref rt (plain-name (uir-var-name node)) node)]
    [(uir-symbol? node) (env-ref rt (uir-symbol-name node) node)]
    [(uir-paren? node) (eval-node rt (uir-paren-inner node))]
    [(uir-block? node) (eval-block rt node)]
    [(uir-call? node) (eval-call rt node)]
    [(uir-let? node) (eval-let rt node)]
    [(uir-set!? node) (eval-set! rt node)]
    [(uir-if? node) (eval-if rt node)]
    [(uir-while? node) (eval-while rt node)]
    [(uir-return? node) (eval-return rt node)]
    [(uir-get? node)
     (error 'run-loop
            "uir-get is interpreted only as an opaque call target; found one in value position: ~s"
            (or (dotted-name node) (safe-tag node)))]
    [else (error 'run-loop "unsupported UIR node tag: ~s" (safe-tag node))]))

(define (eval-block rt node)
  (for/fold ([result #f]) ([stmt (in-list (uir-block-stmts node))])
    (eval-node rt stmt)))

(define (eval-if rt node)
  (if (truthy? (eval-node rt (uir-if-test node)))
      (eval-node rt (uir-if-then node))
      (if (uir-if-else node)
          (eval-node rt (uir-if-else node))
          #f)))

(define (eval-while rt node)
  (define test (uir-while-test node))
  (define body (uir-while-body node))
  (let loop ([iterations 0] [result #f])
    (if (truthy? (eval-node rt test))
        (if (>= iterations LOOP-LIMIT)
            (error 'run-loop
                   "while loop exceeded ~a iterations; the driver likely does not terminate"
                   LOOP-LIMIT)
            (loop (add1 iterations) (eval-node rt body)))
        result)))

(define (eval-let rt node)
  (define name (binding-name (uir-let-name node) 'let-binding))
  (define value (eval-node rt (uir-let-value node)))
  (call-with-bindings rt (list (cons name value))
    (lambda () (eval-node rt (uir-let-body node)))))

(define (eval-set! rt node)
  (define name (binding-name (uir-set!-name node) 'set!-target))
  (define value (eval-node rt (uir-set!-value node)))
  (env-set! rt name value))

(define (eval-return rt node)
  (define value (if (uir-return-value node)
                    (eval-node rt (uir-return-value node))
                    #f))
  (define escape (current-return))
  (if escape
      (escape value)
      (error 'run-loop "uir-return outside a function body; nothing to return from")))

;; ── primitive operators ────────────────────────────────────────────

(define primitive-names
  '("<" ">" "<=" ">=" "=" "!=" "+" "-" "*" "/" "not" "and" "or"))

(define (primitive-name? name)
  (and (member name primitive-names) #t))

(define (check-arity name args expected)
  (if (not (= (length args) expected))
      (error 'run-loop "primitive operator ~a expects ~a argument(s), got ~a"
             name expected (length args))
      (void)))

(define (primitive-and args)
  (cond
    [(null? args) #t]
    [(null? (cdr args)) (car args)]
    [(eq? (car args) #f) #f]
    [else (primitive-and (cdr args))]))

(define (primitive-or args)
  (cond
    [(null? args) #f]
    [(eq? (car args) #f) (primitive-or (cdr args))]
    [else (car args)]))

(define (apply-primitive name args)
  (cond
    [(equal? name "and") (primitive-and args)]
    [(equal? name "or") (primitive-or args)]
    [(equal? name "not") (check-arity name args 1) (not (truthy? (car args)))]
    [(equal? name "=") (apply = args)]
    [(equal? name "!=") (check-arity name args 2) (not (= (first args) (second args)))]
    [(equal? name "+") (apply + args)]
    [(equal? name "-") (apply - args)]
    [(equal? name "*") (apply * args)]
    [(equal? name "/") (apply / args)]
    [(equal? name "<") (apply < args)]
    [(equal? name ">") (apply > args)]
    [(equal? name "<=") (apply <= args)]
    [(equal? name ">=") (apply >= args)]
    [else (error 'run-loop "internal error: unknown primitive operator ~s" name)]))

;; ── calls ──────────────────────────────────────────────────────────

(define (eval-call rt node)
  (define callee (uir-call-callee node))
  (define args (for/list ([a (in-list (uir-call-args node))]) (eval-node rt a)))
  (define name (dotted-name callee))
  (define handler (and name (hash-ref (runtime-handlers rt) name #f)))
  (cond
    ;; Handler precedence: a stub handler shadows a program function of the
    ;; same name, so the effectful body is observed, never executed.
    [(and handler (procedure? handler))
     (record! rt name (apply handler args))]
    [(and name (hash-has-key? (runtime-fns rt) name))
     (apply-fn rt (hash-ref (runtime-fns rt) name) args)]
    [(and name (primitive-name? name))
     (apply-primitive name args)]
    ;; A get chain such as (requests get) is an opaque operation: record it
    ;; under its dotted name and use the handler's value when one is present.
    [(uir-get? callee)
     (if name
         (record! rt name #f)
         (error 'run-loop "cannot resolve opaque call target to a dotted name: ~s"
                (safe-tag callee)))]
    [else
     (error 'run-loop
            "unknown call target: ~s is not a stub handler, manifest function, primitive operator, or get chain"
            (or name (safe-tag callee)))]))

;; ── entry point ────────────────────────────────────────────────────

(define (run-loop program driver-name
                  #:handlers [handlers (hash)]
                  #:state [state (hash)])
  (require! (uir-block? program)
            "program must be a uir-block of named uir-fns, got ~s"
            (safe-tag program))
  (require! (string? driver-name)
            "driver-name must be a string, got ~s" driver-name)
  (require! (hash? handlers)
            "#:handlers must be a hash of string -> procedure, got ~s" handlers)
  (hash-for-each handlers
    (lambda (name handler)
      (require! (and (string? name) (procedure? handler))
                "#:handlers must map strings to procedures; invalid entry ~s -> ~s"
                name handler)))
  (require! (hash? state) "#:state must be a hash of string -> value, got ~s" state)
  (define env (make-hash))
  (hash-for-each state
    (lambda (name value)
      (require! (string? name) "#:state keys must be strings; got ~s" name)
      (hash-set! env name value)))
  (define fns (build-fn-table (uir-block-stmts program)))
  (define driver
    (if (hash-has-key? fns driver-name)
        (hash-ref fns driver-name)
        (error 'run-loop
               "driver function ~s not found in manifest program; available functions: ~s"
               driver-name (sort (hash-keys fns) string<?))))
  (define rt (runtime env (box '()) (box 0) handlers fns))
  (apply-fn rt driver (driver-arguments rt driver))
  (values (reverse (unbox (runtime-events rt))) (hash-copy env)))

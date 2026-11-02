#lang racket

;; effervesce/harness/manifest.rkt
;;
;; P5.3 — Loop as data: the loop manifest.
;;
;; The agentic workflow is data, not Racket control flow. A manifest holds:
;;
;;   name             the manifest's name
;;   manifest-version the manifest schema version (migrations are explicit)
;;   contract         a uir-eff-system: the capabilities the loop may use and
;;                    which loop stage (`component`) needs which capability
;;   program          a uir-block of named uir-fns: one per loop stage plus the
;;                    `driver` function whose body IS the loop
;;   driver           the name of the driver function (the loop entry point)
;;   interps          capability name -> host effect names, the interpretation
;;                    table the bridge gate compares inferred rows against
;;
;; `manifest->uir` / `uir->manifest` map the struct to a UIR node; the versioned
;; UIR envelope and JSON file form live in serialize.rkt. A rewrite of the loop
;; is a rewrite of this UIR value, installed only through manifest-gate.rkt.

(require racklr/uir
         "shape.rkt"
         "run.rkt")

(provide
 (struct-out loop-manifest)
 MANIFEST-SCHEMA-VERSION
 manifest->uir uir->manifest
 base-loop-manifest
 manifest-step-names manifest-component-names manifest-function
 manifest-function-names manifest-interps-hash manifest-loop-shape
 manifest-with-driver-body manifest-with-step-body
 run-manifest step-event step-event? step-event-name step-event-value
 loop-var-set! loop-var-ref call-of op-of fn-of)

(define MANIFEST-SCHEMA-VERSION 1)

(struct loop-manifest (name manifest-version contract program driver interps)
  #:transparent)

;; ── small UIR shorthands used by the base manifest and by tests ─────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (op-of base field) (uir-get base (sym field)))
(define (call-of callee args) (uir-call callee args))
(define (fn-of name body)
  (uir-fn (sym name) '() body #f #f))

(define (loop-var-ref name) (v name))
(define (loop-var-set! name value) (uir-set! (sym name) value))

;; ── converters ─────────────────────────────────────────────────────

(define (effect-name-list->uir effects)
  (uir-list (map (lambda (e) (sym (symbol->string e))) effects)))

(define (interps->uir interps)
  (uir-record
   (for/list ([entry (in-list interps)])
     (cons (uir-string (car entry))
           (effect-name-list->uir (cdr entry))))))

(define (manifest->uir m)
  (unless (loop-manifest? m)
    (error 'manifest->uir "not a loop-manifest: ~e" m))
  (uir-record
   (list (cons (uir-string "name") (uir-string (loop-manifest-name m)))
         (cons (uir-string "manifest-version")
               (uir-number (number->string (loop-manifest-manifest-version m))))
         (cons (uir-string "contract") (loop-manifest-contract m))
         (cons (uir-string "program") (loop-manifest-program m))
         (cons (uir-string "driver") (sym (loop-manifest-driver m)))
         (cons (uir-string "interps") (interps->uir (loop-manifest-interps m))))))

(define (record-ref node key)
  (define entry
    (for/or ([entry (in-list (uir-record-entries node))])
      (and (equal? (uir-string-value (car entry)) key) entry)))
  (unless entry
    (error 'uir->manifest "manifest record is missing ~a" key))
  (cdr entry))

(define (expect-uir pred who what node)
  (unless (pred node)
    (error who "manifest field ~a has the wrong UIR tag: ~e" what node))
  node)

(define (effect-name-list<-uir node)
  (expect-uir uir-list 'uir->manifest 'interps-value node)
  (for/list ([item (in-list (uir-list-items node))])
    (unless (uir-symbol? item)
      (error 'uir->manifest "interps entry is not a symbol: ~e" item))
    (string->symbol (uir-symbol-name item))))

(define (interps<-uir node)
  (expect-uir uir-record 'uir->manifest 'interps node)
  (for/list ([entry (in-list (uir-record-entries node))])
    (unless (uir-string? (car entry))
      (error 'uir->manifest "interps key is not a string: ~e" (car entry)))
    (cons (uir-string-value (car entry))
          (effect-name-list<-uir (cdr entry)))))

(define (manifest-version<-uir node)
  (expect-uir uir-number 'uir->manifest 'manifest-version node)
  (define parsed (string->number (uir-number-value node)))
  (unless (and parsed (exact-nonnegative-integer? parsed))
    (error 'uir->manifest "manifest-version is not an integer: ~e" node))
  (unless (= parsed MANIFEST-SCHEMA-VERSION)
    (error 'uir->manifest
           "unsupported manifest schema version ~a (current: ~a)"
           parsed MANIFEST-SCHEMA-VERSION))
  parsed)

(define (uir->manifest u)
  (expect-uir uir-record 'uir->manifest 'manifest u)
  (define driver-node (record-ref u "driver"))
  (unless (uir-symbol? driver-node)
    (error 'uir->manifest "manifest driver is not a symbol: ~e" driver-node))
  (define name-node (record-ref u "name"))
  (unless (uir-string? name-node)
    (error 'uir->manifest "manifest name is not a string: ~e" name-node))
  (define program (record-ref u "program"))
  (expect-uir uir-block? 'uir->manifest 'program program)
  (loop-manifest
   (uir-string-value name-node)
   (manifest-version<-uir (record-ref u "manifest-version"))
   (expect-uir uir-eff-system? 'uir->manifest 'contract (record-ref u "contract"))
   program
   (uir-symbol-name driver-node)
   (interps<-uir (record-ref u "interps"))))

;; ── accessors over the data ─────────────────────────────────────────

(define (program-fns m)
  (define program (loop-manifest-program m))
  (unless (uir-block? program)
    (error 'manifest-step-names "manifest program is not a uir-block: ~e" program))
  (for/list ([stmt (in-list (uir-block-stmts program))])
    (unless (uir-fn? stmt)
      (error 'manifest-step-names
             "manifest program holds a non-function statement: ~e" stmt))
    (unless (uir-fn-name stmt)
      (error 'manifest-step-names "manifest program holds an unnamed function"))
    stmt))

(define (manifest-function-names m)
  (for/list ([fn (in-list (program-fns m))])
    (uir-symbol-name (uir-fn-name fn))))

(define (manifest-step-names m)
  (define driver (loop-manifest-driver m))
  (for/list ([name (in-list (manifest-function-names m))] #:unless (equal? name driver))
    name))

(define (manifest-function m name)
  (for/or ([fn (in-list (program-fns m))])
    (and (equal? (uir-symbol-name (uir-fn-name fn)) name) fn)))

(define (manifest-component-names m)
  (for/list ([decl (in-list (uir-eff-system-decls (loop-manifest-contract m)))]
             #:when (uir-eff-component? decl))
    (uir-symbol-name (uir-eff-component-name decl))))

(define (manifest-interps-hash m)
  (for/hash ([entry (in-list (loop-manifest-interps m))])
    (values (car entry) (cdr entry))))

(define (manifest-loop-shape m)
  (loop-shape (loop-manifest-program m) (loop-manifest-driver m)))

;; ── rewrites ────────────────────────────────────────────────────────
;;
;; A rewrite is a data edit: replace one stage body or the driver body. The
;; result is a new manifest value; it only becomes the installed loop through
;; gate-manifest. These helpers exist so the rewrite in a test (or by an agent)
;; is visibly a change to the manifest's data, not to harness code.

(define (replace-function-body m fn-name body)
  (unless (manifest-function m fn-name)
    (error 'manifest-rewrite "manifest has no function named ~a" fn-name))
  (define program
    (uir-block
     (for/list ([fn (in-list (program-fns m))])
       (if (equal? (uir-symbol-name (uir-fn-name fn)) fn-name)
           (struct-copy uir-fn fn [body body])
           fn))))
  (struct-copy loop-manifest m [program program]))

(define (manifest-with-driver-body m body)
  (replace-function-body m (loop-manifest-driver m) body))

(define (manifest-with-step-body m step-name body)
  (unless (member step-name (manifest-step-names m))
    (error 'manifest-with-step-body "~a is not a step of this manifest" step-name))
  (replace-function-body m step-name body))

;; ── the base agentic loop ───────────────────────────────────────────

(define (capability name op-name)
  (uir-eff-capability
   (sym name) '()
   (list (uir-eff-capability-op (sym op-name) '() (sym "Unit")))))

(define (given-capabilities . names)
  (uir-eff-given (map (lambda (n) (uir-eff-ref (sym n) '())) names)))

(define (needs . names)
  (map (lambda (n) (uir-eff-ref (sym n) '())) names))

(define (component name needed)
  (uir-eff-component (sym name) (apply needs needed) '() '()))

(define board-cap (capability "Board" "read-issues"))
(define model-cap (capability "Model" "complete"))
(define repo-cap (capability "Repo" "write-file"))
(define shell-cap (capability "Shell" "run-tests"))
(define clock-cap (capability "Clock" "now"))

(define (base-contract)
  (uir-eff-system
   (sym "AgentLoop")
   (list board-cap model-cap repo-cap shell-cap clock-cap
         (given-capabilities "Board" "Model" "Repo" "Shell" "Clock")
         (component "observe" '("Board"))
         (component "decide" '("Model"))
         (component "act" '("Repo"))
         (component "verify" '("Shell"))
         (component "tick" '("Clock"))
         (component "driver" '("Board" "Model" "Repo" "Shell" "Clock")))))

;; One loop stage per function. A stage body performs the concrete host effect
;; the stage needs; the stage is otherwise opaque data to the harness.
(define (base-program)
  (uir-block
   (list
    (fn-of "observe"
           (call-of (op-of (v "requests") "get")
                    (list (uir-string "https://api.github.com/board"))))
    (fn-of "decide"
           (call-of (op-of (v "llm") "query")
                    (list (uir-string "pick the next task"))))
    (fn-of "act"
           (call-of (v "open")
                    (list (uir-string "work.log") (uir-string "w"))))
    (fn-of "verify"
           (call-of (op-of (v "subprocess") "run")
                    (list (uir-string "racket tests"))))
    (fn-of "tick"
           (call-of (op-of (v "time") "time") '()))
    ;; The whole loop lives here, as data: wake, tick, then a bounded set of
    ;; decide/act/verify rounds. `round` is loop state seeded by the runner.
    (fn-of "driver"
           (uir-block
            (list
             (call-of (v "observe") '())
             (call-of (v "tick") '())
             (uir-while
              (call-of (sym "<") (list (v "round") (uir-number "3")))
              (uir-block
               (list
                (call-of (v "decide") '())
                (call-of (v "act") '())
                (call-of (v "verify") '())
                (call-of (v "tick") '())
                (loop-var-set! "round"
                               (call-of (sym "+")
                                        (list (v "round") (uir-number "1"))))))
              (uir-null))))))))

(define (base-interps)
  (list (cons "Board" '(network))
        (cons "Model" '(llm))
        (cons "Repo" '(io))
        (cons "Shell" '(process))
        (cons "Clock" '(time))))

(define (base-loop-manifest)
  (loop-manifest "agent-loop" MANIFEST-SCHEMA-VERSION
                 (base-contract) (base-program) "driver" (base-interps)))

;; ── running the loop ────────────────────────────────────────────────
;;
;; The runner is generic UIR interpretation: it reads the driver out of the
;; manifest's data, so the executed loop is whatever the manifest says. Loop
;; state (the `round` counter in the base loop) is seeded through #:state; each
;; stage call is dispatched to a stub handler and recorded as a step-event.
(define (run-manifest m #:handlers [handlers (hash)] #:state [state (hash)])
  (run-loop (loop-manifest-program m)
            (loop-manifest-driver m)
            #:handlers handlers
            #:state state))

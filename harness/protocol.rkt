#lang racket

;; effervesce/harness/protocol.rkt
;;
;; The eval surface's wire protocol: one JSON object per line in, one JSON
;; object per line out. The process is long-lived, so a request either
;; answers a query against the live system or redefines it in place.
;;
;; Requests:
;;   {"op":"stat"}
;;   {"op":"eval","name":"worker","text":"system WorkerLive { ... }"}
;;   {"op":"remove","name":"worker"}
;;   {"op":"ask","question":["capabilities-of","App"]}
;;   {"op":"check"}
;;   {"op":"verify"}
;;   {"op":"shutdown"}
;;
;; Every response carries `pid` and `generation`, so a caller can see that
;; the same process served the query before and after a redefinition.

(require json
         "../check.rkt"
         "../check/query.rkt"
         "../check/verify.rkt"
         "process.rkt")

(provide handle-line)

;; ── small JSON helpers ──────────────────────────────────────────────

;; A capability reference as the checkers use it: (name . type-args), where
;; the checker's `ref-key` carries plain strings.
(define (ref-key? x)
  (and (pair? x)
       (string? (car x))
       (list? (cdr x))
       (andmap string? (cdr x))))

;; Render a System 1 answer value as JSON. Answers are ref-keys, symbols,
;; booleans, or lists of those; anything else falls back to its printed form.
(define (json-value v)
  (cond
    [(boolean? v) v]
    [(number? v) v]
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [(set? v) (json-value (set->list v))]
    [(list? v)
     (cond
       [(andmap ref-key? v) (map format-ref-key v)]
       [(andmap symbol? v) (map symbol->string v)]
       [(andmap string? v) v]
       [else (map (λ (x) (format "~a" x)) v)])]
    [else (format "~a" v)]))

;; JSON object keys become symbols when a line is read, so a request field is
;; fetched by symbol. A missing or wrong-typed field is a protocol error.
(define (require-field req key kind)
  (define v (hash-ref req key #f))
  (unless (and v (kind v))
    (error 'harness-protocol "request needs a valid ~a field named ~a"
           (if (eq? kind string?) "string" "array")
           key))
  v)

;; ── questions ───────────────────────────────────────────────────────

(define (stringify-part x)
  (cond
    [(string? x) x]
    [(symbol? x) (symbol->string x)]
    [else (format "~a" x)]))

;; Translate the JSON question array into the structured question `ask`
;; dispatches on. Mirrors the CLI's question vocabulary.
(define (question-from-json js)
  (define parts (map stringify-part js))
  (match parts
    [(list "capabilities-of" cname) (list 'capabilities-of cname)]
    [(list "effects-of" cname) (list 'effects-of cname)]
    [(list "side-effects-of" cname) (list 'side-effects-of cname)]
    [(list "touches" resource) (list 'touches resource)]
    [(list "resources-of" eff) (list 'resources-of eff)]
    [(list "reachable?" cap) (list 'reachable? (cons cap '()))]
    [(cons op args) (list (string->symbol op) (map string->symbol args))]
    [_ (error 'harness-protocol "question must be a non-empty array")]))

;; ── responses ───────────────────────────────────────────────────────

(define (base-response h)
  (hash 'ok #t
        'pid (harness-pid h)
        'generation (harness-generation h)))

(define (error-response h message)
  (hash 'ok #f
        'pid (harness-pid h)
        'generation (harness-generation h)
        'error message))

(define (answer-response h a)
  (define conf (symbol->string (answer-confidence a)))
  (define base (hash-set* (base-response h)
                          'confidence conf
                          'value (json-value (answer-value a))))
  (if (answer-note a)
      (hash-set base 'note (format "~a" (answer-note a)))
      base))

(define (definition-response h redefined?)
  (hash-set* (base-response h)
             'redefined redefined?
             'definitions (harness-definition-names h)
             'decls (decl-kind-counts (harness-system h))))

(define (stat-response h)
  (hash-set (harness-describe h) 'ok #t))

;; ── dispatch ────────────────────────────────────────────────────────

;; Handle one request line. Returns (values response new-harness shutdown?).
;; A malformed request becomes an error response; it never ends the process.
(define (handle-line h line)
  (with-handlers ([exn:fail?
                   (λ (e) (values (error-response h (exn-message e)) h #f))])
    (define req (string->jsexpr line))
    (unless (hash? req)
      (error 'harness-protocol "a request must be a JSON object"))
    (handle-request h req)))

(define (handle-request h req)
  (define op (require-field req 'op string?))
  (cond
    [(string=? op "stat")
     (values (stat-response h) h #f)]
    [(string=? op "eval")
     (define name (require-field req 'name string?))
     (define text (require-field req 'text string?))
     (define redefined? (for/or ([n (in-list (harness-definition-names h))])
                          (string=? n name)))
     (define h* (harness-eval h name text))
     (values (definition-response h* redefined?) h* #f)]
    [(string=? op "remove")
     (define name (require-field req 'name string?))
     (define h* (harness-remove h name))
     (values (definition-response h* #t) h* #f)]
    [(string=? op "ask")
     (define q (question-from-json (require-field req 'question list?)))
     (values (answer-response h (harness-query h q)) h #f)]
    [(string=? op "check")
     (define v (harness-check h))
     (values (hash-set* (base-response h)
                        'check-ok (check-ok? v)
                        'report (check-report v))
             h #f)]
    [(string=? op "verify")
     (define findings (harness-verify h))
     (values (hash-set* (base-response h)
                        'verify-ok (verify-ok? findings)
                        'report (verify-report findings))
             h #f)]
    [(string=? op "shutdown")
     (values (hash-set (base-response h) 'event "shutdown") h #t)]
    [else (error 'harness-protocol "unknown op: ~a" op)]))

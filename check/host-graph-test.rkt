#lang racket

;; Recovery tests for effervesce/check/host-graph.rkt: does the projection from
;; inferred host rows to the capability-level graph contain exactly the
;; nodes/links its documented reading of each row entry produces?

(require rackunit
         racklr/uir
         racklr/hypergraph
         "host-graph.rkt")

;; ── synthetic host program + rows ───────────────────────────────────

(define (v s) (uir-var (uir-symbol s)))
(define (attr base f) (uir-get base (uir-string f)))
(define (prog . stmts) (uir-block stmts))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (method name body) (uir-method (uir-symbol name) '() body 'public))
(define (cls name methods) (uir-class (uir-symbol name) '() '() methods))
(define (num n) (uir-number n))

(define (entry name modality ops [scope #f])
  (uir-effect-entry (uir-symbol name) modality
                    (map uir-symbol ops)
                    #f
                    (and scope (uir-symbol scope))))

(define (row . entries) (uir-effect-row entries))

;; Store.add writes an instance attribute (durable state); reader/handler only
;; write locals (bindings, not states).
(define program
  (prog (cls "Store"
             (list (method "add" (prog (uir-set! (attr (v "self") "cache") (num 1))))))
        (fn "reader" (prog (uir-set! (v "tmp") (num 2))))
        (fn "handler" (prog (uir-set! (v "n") (num 3))))))

(define component "host/mod.py")

(define rows
  (hash component
        (hash 'handler (row (entry "db" 'may '())
                            (entry "network" 'may '("network<Session>.send"))
                            (entry "io" 'may '() "reads=idx")
                            (entry "unknown" 'unknown '()))
              'Store.add (row (entry "db" 'may '()))
              'reader (row))))

(define atoms (host-capability-atoms (hash component program) rows))

(define (atom-links type)
  (for/list ([a (in-list atoms)]
             #:when (and (link? a) (eq? (link-type a) type)))
    a))

(define (out-names l) (map node-name (link-out l)))

(define (has-link? type pair)
  (for/or ([l (in-list (atom-links type))])
    (equal? (out-names l) pair)))

;; ── component-needs: coarse entry -> domain capability; typed op -> class ──

(define (test-needs)
  (define needs (atom-links 'component-needs))
  ;; db coarse, io coarse (scope-carrying but still coarse), Session typed
  (check-equal? (length needs) 3)
  (check-true (has-link? 'component-needs (list component (cons 'io '()))))
  (check-true (has-link? 'component-needs (list component (cons 'db '()))))
  (check-true (has-link? 'component-needs (list component (cons 'Session '()))))
  ;; the unknown entry is a tracked obligation, never a capability
  (check-false (has-link? 'component-needs (list component (cons 'unknown '())))))

;; ── capability-effect: domain -> domain, class -> domain ────────────

(define (test-capability-effect)
  (define ces (atom-links 'capability-effect))
  (check-equal? (length ces) 4)
  (check-true (has-link? 'capability-effect (list (cons 'db '()) 'db)))
  (check-true (has-link? 'capability-effect (list (cons 'io '()) 'io)))
  (check-true (has-link? 'capability-effect (list (cons 'Session '()) 'network)))
  ;; Store's own method row performs db, so the provided class entails it
  (check-true (has-link? 'capability-effect (list (cons 'Store '()) 'db))))

;; ── provides: each class the component defines ──────────────────────

(define (test-provides)
  (define ps (atom-links 'component-provides))
  (check-equal? (length ps) 1)
  (check-true (has-link? 'component-provides (list component (cons 'Store '())))))

;; ── effect-touches: typed-op receiver + scope + coarse component ────

(define (test-touches)
  (define ts (atom-links 'effect-touches))
  (check-equal? (length ts) 3)
  (check-true (has-link? 'effect-touches (list 'network 'Session)))
  (check-true (has-link? 'effect-touches (list 'io 'reads=idx)))
  ;; the coarse db entry names no finer resource: the component is the
  ;; coarsest resource the row proves the effect is performed in
  (check-true (has-link? 'effect-touches (list 'db (string->symbol component)))))

;; ── action-writes: durable (non-local) writes only ──────────────────

(define (test-action-writes)
  (define ws (atom-links 'action-writes))
  (check-equal? (length ws) 1)
  (check-true (has-link? 'action-writes
                         (list (format "~a::~a" component "Store.add")
                               (format "~a::~a" component "Store.self.cache"))))
  ;; local bindings are not state writes
  (check-false (for/or ([l (in-list ws)])
                 (regexp-match? #rx"tmp|handler" (format "~a" (out-names l))))))

;; ── a component with no rows yields no capability edges ─────────────

(define (test-empty)
  (check-equal? (host-capability-atoms (hash) (hash)) '())
  ;; a program with no rows still provides its classes, but recovers no
  ;; row-derived edges; an empty program recovers nothing at all
  (check-equal? (host-capability-atoms (hash component (prog)) (hash)) '())
  (check-equal? (map link-type (host-capability-atoms (hash component program) (hash)))
                '(component-provides action-writes)))

(module+ main
  (test-needs)
  (test-capability-effect)
  (test-provides)
  (test-touches)
  (test-action-writes)
  (test-empty)
  (displayln "All host-graph tests passed."))

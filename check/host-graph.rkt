#lang racket

;; effervesce/check/host-graph.rkt
;;
;; Recover the capability-level graph from inferred host effect rows.
;;
;; A .fizz system declares its capability graph (`component C needs R`,
;; `provides`, `given`, handlers, actions). Host source code has no such
;; declarations, so `check/graph.rkt`'s host projection used to populate only
;; the effect half of the graph (`fn-effects`). This module projects the other
;; half from the one thing host code does give us: the inferred function rows.
;;
;; The recovery is the contract reading of each row entry, using the same
;; capability identity the checkers use (a `ref-key`, name + type arguments):
;;
;;   * An entry with empty `ops` means "any operation in this domain" (the
;;     documented coarse entry). The component needs the domain capability
;;     `domain` (a bare ref-key), and that capability is interpreted by the
;;     domain effect: `capability-effect  domain -> domain`.
;;   * An entry with typed ops `domain<Class>.verb` names the concrete class
;;     that performs the operation. The component needs capability `Class`,
;;     and `Class` is interpreted by the entry's effect domain:
;;     `capability-effect  Class -> domain`.
;;   * A component provides the capability of every class it defines: a class
;;     is the unit that implements a capability's operations. The class
;;     capability is interpreted by the effects its own method rows perform,
;;     so a provided capability carries the domain it entails.
;;   * `effect-touches`: a typed op touches its receiver class as the concrete
;;     resource, and a scope-carrying entry touches that scope. A coarse entry
;;     with neither names no finer resource, so the performing component is the
;;     coarsest resource the effect touches (the row still proves the effect is
;;     performed in that component).
;;   * `action-writes`: a named function/method that writes a non-local state
;;     target (an attribute path, not a local binding) is an action; the
;;     written target is the state it writes.
;;
;; Effect domains are only known at the classifier's granularity: the bare
;; domain name (`db`, `io`, ...) is the capability when the row is coarse, and
;; the receiver class name (`FileProxyMixin`, ...) is the capability when the
;; row is precise. `unknown` entries are a tracked inference obligation, not a
;; capability, so they are never projected as needs or interpretations.
;;
;; The projection is intentionally independent of any declared `.fizz` spec:
;; `check/graph.rkt` applies it only to host components a spec does not
;; declare, so declared capability declarations stay authoritative.

(require racklr/uir
         racklr/hypergraph
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/collect
         racklr/effects/walk
         racklr/effects/shape)

(provide host-capability-atoms)

;; ── row/entry access ────────────────────────────────────────────────

;; A component's function names, sorted so the projection is deterministic.
(define (component-fn-names rows c)
  (sort (hash-keys (hash-ref rows c)) symbol<?))

;; Every recorded effect entry of every function row, as (component . entry).
(define (all-row-entries rows)
  (append-map
   (λ (c)
     (append-map
      (λ (fn) (map (λ (e) (cons c e))
                   (uir-effect-row-entries (hash-ref (hash-ref rows c) fn))))
      (component-fn-names rows c)))
   (component-names rows)))

(define (component-names h)
  (sort (hash-keys h) string<?))

;; The effect domain an entry records, or #f for the deferred `unknown`
;; obligation (which is not a capability the code needs).
(define (entry-domain e)
  (define n (effect-name-symbol e))
  (and (not (eq? n 'unknown)) n))

;; Parsed parts (domain receiver verb) of each typed op on an entry.
(define (entry-op-parts e)
  (filter values (map typed-op-parts (entry-ops e))))

;; ── needs + capability-effect ───────────────────────────────────────

;; component-needs / capability-effect edges recovered from the rows.
(define (needs-and-interpretation-atoms rows)
  (append-map
   (λ (pair)
     (define c (car pair))
     (define e (cdr pair))
     (define domain (entry-domain e))
     (cond
       [(not domain) '()]
       [(null? (uir-effect-entry-ops e))
        ;; Coarse entry: the component needs the domain capability itself,
        ;; interpreted directly by the domain effect.
        (list (hg-component-needs-link c (cons domain '()))
              (hg-capability-effect-link domain domain))]
       [else
        ;; Precise entry: the component needs the concrete class that
        ;; performs the operation, interpreted by the entry's domain.
        (append-map
         (λ (parts)
           (define cls (string->symbol (second parts)))
           (list (hg-component-needs-link c (cons cls '()))
                 (hg-capability-effect-link cls (first parts))))
         (entry-op-parts e))]))
   (all-row-entries rows)))

;; ── provides + class interpretation ─────────────────────────────────

;; Components provide a capability for each class they define. The class's
;; method rows (keyed `Class.method` by `collect-methods`) say which effect
;; domains implementing that capability entails.
(define (component-classes program)
  (sort (hash-keys (collect-class-names program)) symbol<?))

(define (class-domains rows c cls)
  (define rx (regexp (format "^~a[.]" (regexp-quote (symbol->string cls)))))
  (remove-duplicates
   (sort
    (for*/list ([fn (in-list (component-fn-names rows c))]
                #:when (regexp-match? rx (symbol->string fn))
                [e (in-list (uir-effect-row-entries (hash-ref (hash-ref rows c) fn)))]
                [d (in-value (entry-domain e))]
                #:when d)
      d)
    symbol<?)))

(define (provides-atoms host-code rows)
  (append-map
   (λ (c)
     (define program (hash-ref host-code c #f))
     (if (not program)
         '()
         (append-map
          (λ (cls)
            (cons (hg-component-provides-link c (cons cls '()))
                  (map (λ (d) (hg-capability-effect-link cls d))
                       (if (hash-has-key? rows c)
                           (class-domains rows c cls)
                           '()))))
          (component-classes program))))
   (component-names host-code)))

;; ── effect-touches ──────────────────────────────────────────────────

;; The concrete resources one entry touches: its scope, or the receiver class
;; of each typed op. An entry that names neither resource falls back to the
;; component itself — the coarsest granularity the row still proves (the effect
;; is performed somewhere in that component), never a guessed finer scope.
(define (touch-atoms-entry c e)
  (define domain (entry-domain e))
  (cond
    [(not domain) '()]
    [else
     (define scoped
       (if (uir-effect-entry-scope e)
           (list (hg-effect-touches-link
                  domain
                  (string->symbol (uir-symbol-name (uir-effect-entry-scope e)))))
           '()))
     (define typed
       (map (λ (parts) (hg-effect-touches-link (first parts)
                                               (string->symbol (second parts))))
            (entry-op-parts e)))
     (cond
       [(pair? (append scoped typed)) (append scoped typed)]
       [else (list (hg-effect-touches-link domain (string->symbol c)))])]))

(define (touches-atoms rows)
  (append-map (λ (pair) (touch-atoms-entry (car pair) (cdr pair)))
              (all-row-entries rows)))

;; ── action-writes ───────────────────────────────────────────────────

;; Dotted target name of a write that mutates durable state, or #f for a local
;; binding (the model checker's `state := expr` step is a durable write, not a
;; let-binding) or a target whose chain is not a plain attribute path.
(define (written-state target)
  (define (chain e acc)
    (cond
      [(uir-var? e) (and (pair? acc) (string-join (cons (uir-symbol-name (uir-var-name e)) acc) "."))]
      [(uir-get? e)
       (define f (get-field-symbol (uir-get-field e)))
       (and f (chain (uir-get-base e) (cons (symbol->string f) acc)))]
      [else #f]))
  (and (uir-get? target) (chain target '())))

;; State identity: the component, plus the enclosing class for a method (so
;; `self.x` in two classes of one module stays two states), plus the target.
(define (state-name component fname target)
  (define m (regexp-match #rx"^([^.]+)[.]" (symbol->string fname)))
  (if m
      (format "~a::~a.~a" component (list-ref m 1) target)
      (format "~a::~a" component target)))

;; set! targets written anywhere in a function body (nested functions are
;; their own body and are collected separately, so their writes do not leak).
(define (body-written-states body)
  (define out '())
  (walk-body body (λ (n) (when (uir-set!? n)
                           (define s (written-state (uir-set!-name n)))
                           (when s (set! out (cons s out))))))
  out)

;; action-writes edges for every named function and method that writes state.
(define (action-atoms host-code)
  (append-map
   (λ (c)
     (define program (hash-ref host-code c #f))
     (if (not program)
         '()
         (append-map
          (λ (pair)
            (define fname (car pair))
            (define action (format "~a::~a" c fname))
            (for/list ([state (in-list (body-written-states (cdr pair)))])
              (hg-action-writes-link action (state-name c fname state))))
          (append (hash->list (collect-functions program))
                  (hash->list (collect-methods program))))))
   (component-names host-code)))

;; ── projection ──────────────────────────────────────────────────────

;; host-code : (hash component-name uir-program)
;; rows      : (hash component-name (hash function-name-symbol uir-effect-row))
;;             — the same component→rows shape `infer-effects` returns per
;;             component; a component with no row entry contributes no edges.
;;
;; Returns the capability-level atoms recovery can justify from the rows: the
;; capability/effect edges (`component-needs`, `component-provides`,
;; `capability-effect`) plus the resource and mutation edges
;; (`effect-touches`, `action-writes`). Nodes are carried by the links.
(define (host-capability-atoms host-code rows)
  (remove-duplicates
   (append (needs-and-interpretation-atoms rows)
           (provides-atoms host-code rows)
           (touches-atoms rows)
           (action-atoms host-code))))

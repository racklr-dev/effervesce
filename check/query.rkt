#lang racket

;; effervesce/check/query.rkt
;;
;; System 1: fast, closed-form questions over the indexed system graph.
;; Every answer carries a confidence tag. `certain` means the value came
;; from an index lookup or a memoized transitive closure; `unknown` means
;; the question cannot be answered from the provided graph and the caller
;; should escalate to System 2 (verify) or supply another artifact.
;;
;; System 1 never guesses: it returns `unknown` rather than an empty or
;; partial answer that could be misread as a negative result.

(require racklr/hypergraph
         "graph.rkt")

(provide
  answer answer? answer-value answer-confidence answer-note
  certain unknown
  ask
  capabilities-of effects-of side-effects-of touches resources-of
  reachable? reachable-caps
  calls-of
  format-ref-key
  host-level-question?)

;; ── answer ──────────────────────────────────────────────────────────

;; confidence is 'certain | 'unknown. `note` explains an `unknown`.
(struct answer (value confidence note) #:transparent)

(define (certain v) (answer v 'certain #f))
(define (unknown v note) (answer v 'unknown note))

;; ── closed-form queries ─────────────────────────────────────────────

;; Capabilities a component `needs` (list of ref-keys).
(define (capabilities-of idx cname)
  (for/list ([l (in-list (index-links idx 'component-needs))]
             #:when (equal? (node-name (first (link-out l))) cname))
    (node-name (second (link-out l)))))

;; Host effects inferred for a component's code (list of effect-name symbols).
(define (effects-of idx cname)
  (for/list ([l (in-list (index-links idx 'fn-effects))]
             #:when (equal? (node-name (first (link-out l))) cname))
    (node-name (second (link-out l)))))

;; Effects a capability maps to through the bridge interpretation table.
(define (capability-effects idx cap)
  (for/list ([l (in-list (index-links idx 'capability-effect))]
             #:when (equal? (node-name (first (link-out l))) cap))
    (node-name (second (link-out l)))))

;; Effects that touch a given resource/scope (the DualLink inverse).
(define (touches idx resource)
  (for/list ([l (in-list (index-links idx 'effect-touches))]
             #:when (equal? (node-name (second (link-out l))) resource))
    (node-name (first (link-out l)))))

;; Resources an effect touches.
(define (resources-of idx eff-name)
  (for/list ([l (in-list (index-links idx 'effect-touches))]
             #:when (equal? (node-name (first (link-out l))) eff-name))
    (node-name (second (link-out l)))))

;; ── materialized AST queries ────────────────────────────────────────

;; The atom at slot position `i` of a materialized vertex (one slot link hop).
(define (slot-child idx vertex i)
  (for/or ([l (in-list (index-links idx 'slot))]
           #:when (equal? (first (link-out l)) vertex))
    (and (= (node-name (second (link-out l))) i)
         (third (link-out l)))))

;; Resolve a materialized vertex to its leaf scalar (following slot 0 down
;; through single-child wrappers like uir-symbol/uir-var), or #f.
(define (leaf-of idx a)
  (let loop ([a a] [depth 0])
    (cond
      [(and (node? a) (memq (node-type a) '(sym str num bool nil)))
       (node-name a)]
      [(and (node? a) (< depth 64))
       (define c (slot-child idx a 0))
       (and c (loop c (add1 depth)))]
      [else #f])))

;; Every operation/function name invoked by materialized host ASTs.
(define (calls-of idx)
  (remove-duplicates
   (filter values
     (for/list ([cv (in-list (index-nodes idx 'uir-call))])
       (define callee (slot-child idx cv 0))
       (cond
         [(and (node? callee) (memq (node-type callee) '(uir-symbol uir-var)))
          (leaf-of idx callee)]
         [(and (node? callee) (eq? (node-type callee) 'uir-get))
          (leaf-of idx (slot-child idx callee 1))]
         [else #f])))))

;; ── handling-graph traversal (via + provides + needs) ───────────────

;; The capability-handling graph, indexed for traversal. Mirrors the type
;; checker's `given-keys` / `via-edges` / `providers-by-key` / `provider-needs`.
(struct handling (givens via-targets providers-by-key needs-of) #:transparent)

(define (edge-map idx type kpos vpos)
  (define h (make-hash))
  (for ([l (in-list (index-links idx type))])
    (hash-update! h (node-name (list-ref (link-out l) kpos))
                  (λ (vs) (cons (node-name (list-ref (link-out l) vpos)) vs))
                  '()))
  h)

(define (handling-structure idx)
  (handling
   (list->set
    (map (λ (l) (node-name (first (link-out l)))) (index-links idx 'given)))
   (edge-map idx 'capability-interprets 0 1)
   (edge-map idx 'component-provides 1 0)
   (edge-map idx 'component-needs 0 1)))

;; Is capability `cap` (a ref-key) handled: does a path exist through `via`
;; interpretations and peer `provides` down to a `given`? Memoized DFS with
;; per-path cycle detection. Matches the type checker's `handled?` for the
;; single-handler-per-capability case the examples use.
(define (reachable? idx cap)
  (define h (handling-structure idx))
  (define memo (make-hash))
  (define visiting (make-hash))
  (define (go c)
    (cond
      [(hash-ref visiting c #f) #f]              ; cycle on this path
      [(hash-has-key? memo c) (hash-ref memo c)]
      [else
       (hash-set! visiting c #t)
       (define ts (hash-ref (handling-via-targets h) c '()))
       (define result
         (or (set-member? (handling-givens h) c)
             (and (pair? ts) (andmap go ts))
             (for/or ([p (in-list (hash-ref (handling-providers-by-key h) c '()))])
               (andmap go (hash-ref (handling-needs-of h) p '())))))
       (hash-remove! visiting c)
       (hash-set! memo c result)
       result]))
  (go cap))

;; Every capability on a handling path from `cap`, including `cap` itself
;; (a set). Follows via-interpretations and provider needs to the terminals.
(define (reachable-caps idx cap)
  (define h (handling-structure idx))
  (define memo (make-hash))
  (define visiting (make-hash))
  (define (go c)
    (cond
      [(hash-has-key? memo c) (hash-ref memo c)]
      [(hash-ref visiting c #f) (set)]            ; cycle adds nothing new
      [else
       (hash-set! visiting c #t)
       (define downstream
         (append
          (append-map go (hash-ref (handling-via-targets h) c '()))
          (append-map
           (λ (p) (append-map go (hash-ref (handling-needs-of h) p '())))
           (hash-ref (handling-providers-by-key h) c '()))))
       (define result (set-add (list->set downstream) c))
       (hash-remove! visiting c)
       (hash-set! memo c result)
       result]))
  (go cap))

;; Side effects a component performs: its own inferred effects, plus the
;; effects of every capability on its handling paths (needs, via targets,
;; provider needs, down to given), one hop through the interpretation table.
(define (side-effects-of idx cname)
  (define closure
    (list->set
     (append-map (λ (c) (set->list (reachable-caps idx c)))
                 (capabilities-of idx cname))))
  (remove-duplicates
   (append (effects-of idx cname)
           (append-map (λ (c) (capability-effects idx c)) (set->list closure)))))

;; ── dispatcher ──────────────────────────────────────────────────────

;; `q` is a structured question: (list 'capabilities-of cname), etc.
(define (ask idx q)
  (match q
    [(list 'capabilities-of cname) (certain (capabilities-of idx cname))]
    [(list 'effects-of cname) (certain (effects-of idx cname))]
    [(list 'side-effects-of cname) (certain (side-effects-of idx cname))]
    [(list 'touches resource) (certain (touches idx resource))]
    [(list 'resources-of eff) (certain (resources-of idx eff))]
    [(list 'reachable? cap) (certain (reachable? idx cap))]
    [_ (unknown 'unsupported (format "no such query: ~a" q))]))

;; Questions that need host artifacts (fn-effects, effect-touches) beyond a
;; bare .fizz spec. The CLI reports these as unknown when only a spec is given.
(define (host-level-question? q)
  (and (pair? q) (memq (car q) '(effects-of side-effects-of touches resources-of))))

;; ── formatting ──────────────────────────────────────────────────────

(define (format-ref-key k)
  (define name (car k))
  (define args (cdr k))
  (if (null? args)
      name
      (string-append name "<" (string-join args ", ") ">")))

(module+ main
  (displayln "effervesce/check/query — System 1 query surface loaded."))

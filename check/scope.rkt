#lang racket

;; effervesce/check/scope.rkt
;;
;; Contextual scoping: the typed-traversal kernel every query shares.
;;
;; A query anchors at a node (a component, a capability, an effect, a state,
;; an operation, an AST vertex) and scopes to the subgraph reachable from it
;; along declared edge kinds. `edge-shapes` is the declaration: for each link
;; type in check/graph.rkt, the (from . to) slot pairs a traversal may follow.
;; A `hop` names one declared pair; `follow` walks one hop from a set of
;; nodes; `traverse` chains hops into a scope plan; `closure` iterates a hop
;; set to a fixpoint. `scope-atoms` returns the induced subgraph (the nodes
;; plus the links the walk crossed) and `walk-paths` returns the paths that
;; explain a reach, so "what does this touch" and "show me the chain that
;; makes it so" are the same traversal shape.
;;
;; Only declared edges can be walked: an undeclared edge kind, or a slot pair
;; the vocabulary does not define, is a construction error rather than a
;; silently empty walk.

(require racklr/hypergraph
         "graph.rkt")

(provide
  edge-shapes edge-declared?
  hop hop? hop-type hop-from hop-to
  forward-hop backward-hop slot-hop
  follow traverse closure scope-nodes scope-atoms walk-paths)

(define edge-shapes
  (hash 'component-needs       '((0 . 1) (1 . 0))
        'component-provides    '((0 . 1) (1 . 0))
        'capability-interprets '((0 . 1) (1 . 0))
        'capability-effect     '((0 . 1) (1 . 0))
        'capability-op         '((0 . 1) (1 . 0))
        'component-op          '((0 . 1) (1 . 0))
        'component-call        '((0 . 1) (1 . 0))
        'handler-effect        '((0 . 1) (1 . 0))
        'handler-interprets    '((0 . 1) (1 . 0))
        'handler-op            '((0 . 1) (1 . 0))
        'handler-call          '((0 . 1) (1 . 0))
        'given                 '((0 . 1))
        'action-writes         '((0 . 1) (1 . 0))
        'declares-state        '((0 . 1) (1 . 0))
        'invariant-references  '((0 . 1) (1 . 0))
        'fn-effects            '((0 . 1) (1 . 0))
        'effect-touches        '((0 . 1) (1 . 0))
        'effect-site           '((0 . 1) (1 . 0) (1 . 2))
        'slot                  '((0 . 2))))

;; One declared link-type/slot pair. `hop-from` is the slot the walked node
;; occupies and `hop-to` is the slot the step lands on.
(struct traversal-hop (type from to) #:transparent)

(define hop? traversal-hop?)
(define hop-type traversal-hop-type)
(define hop-from traversal-hop-from)
(define hop-to traversal-hop-to)

;; Guard at construction: a hop can only name a declared edge kind.
(define (hop type from to)
  (unless (edge-declared? type from to)
    (error 'hop "undeclared edge kind/slots: ~a ~a ~a" type from to))
  (traversal-hop type from to))

;; Declared hops over a link type: forward (0→1), backward (1→0), and the
;; UIR materialization slot (0→2, skipping the slot-index node).
(define (forward-hop type) (hop type 0 1))
(define (backward-hop type) (hop type 1 0))
(define (slot-hop) (hop 'slot 0 2))

(define (edge-declared? type from to)
  (and (hash-has-key? edge-shapes type)
       (and (member (cons from to) (hash-ref edge-shapes type)) #t)))

(define (link-matches? l type from to node)
  (and (eq? (link-type l) type)
       (let ([out (link-out l)])
         (and (> (length out) to)
              (equal? (list-ref out from) node)))))

;; Every link of a declared type, in a stable order. A scope walks the
;; declared edge vocabulary; graphs here are spec-sized.
(define (declared-links idx)
  (append-map (λ (t) (index-links idx t)) (index-link-types idx)))

;; One hop from every node in `nodes`. Returns the reached nodes and, when
;; `with-links?` is set, the links the step crossed.
(define (follow* idx nodes hops with-links?)
  (define out-nodes '())
  (define out-links '())
  (for ([node (in-list nodes)])
    (for ([l (in-list (declared-links idx))])
      (for ([h (in-list hops)]
            #:when (link-matches? l (hop-type h) (hop-from h) (hop-to h) node))
        (define landed (list-ref (link-out l) (hop-to h)))
        (set! out-nodes (cons landed out-nodes))
        (when with-links? (set! out-links (cons l out-links))))))
  (values (reverse out-nodes) (reverse out-links)))

(define (follow idx nodes hops)
  (define-values (ns _) (follow* idx nodes hops #f))
  (remove-duplicates ns))

;; Follow a sequence of hop sets; each level is (listof hop).
(define (traverse idx roots steps)
  (for/fold ([nodes (list->set roots)]) ([hops (in-list steps)])
    (list->set (follow idx (set->list nodes) hops))))

;; Contextual scope: every node on a path from `roots` along `steps` — the
;; roots, each intermediate level, and the level the last step lands on.
(define (scope-nodes idx roots steps)
  (define acc (list->set roots))
  (define frontier (remove-duplicates roots))
  (for ([hops (in-list steps)])
    (define next (remove-duplicates (follow idx frontier hops)))
    (set! acc (set-union acc (list->set next)))
    (set! frontier next))
  (set->list acc))

;; The induced subgraph: the nodes in scope and every link the walk crossed.
(define (scope-atoms idx roots steps)
  (define nodes (list->set roots))
  (define crossed '())
  (for ([hops (in-list steps)])
    (define-values (ns ls) (follow* idx (set->list nodes) hops #t))
    (set! crossed (append crossed ls))
    (set! nodes (set-union nodes (list->set ns))))
  (append (set->list nodes) (remove-duplicates crossed)))

;; Fixpoint of one hop set: the closure of `roots` under `hops` (roots
;; included). Cycles are safe because the reached set only grows.
(define (closure idx roots hops)
  (let loop ([seen (list->set roots)] [frontier (remove-duplicates roots)])
    (define next (remove-duplicates (follow idx frontier hops)))
    (define fresh (filter (λ (n) (not (set-member? seen n))) next))
    (cond
      [(null? fresh) (set->list seen)]
      [else (loop (set-union seen (list->set fresh)) fresh)])))

;; Every path from a root to a reachable node, as a list of node chains.
;; Cycle-safe (a node never repeats on a path) and bounded by `#:max-depth`.
(define (walk-paths idx roots steps #:max-depth [max-depth 32])
  (define (walk node steps acc)
    (cond
      [(null? steps) (list (reverse (cons node acc)))]
      [(>= (length acc) max-depth) '()]
      [else
       (append-map (λ (next)
                     (if (member next acc)
                         '()
                         (walk next (cdr steps) (cons node acc))))
                   (follow idx (list node) (car steps)))]))
  (append-map (λ (root) (walk root steps '())) roots))

(module+ main
  (displayln "effervesce/check/scope — contextual scoping kernel loaded."))

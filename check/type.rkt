#lang racket

;; Effect-row reachability plus invocation checking.
;;
;; Two questions, both over the UIR (no runtime semantics):
;;
;;   1. Reachability: is every capability a component asks for (`needs`)
;;      backed by a `given`, possibly through `via` interpretations?
;;   2. Invocations: does each component body only invoke capability
;;      operations whose owning capability is in that component's `needs`?
;;
;; A `component` body may also call plain functions (like `parse`) that no
;; capability declares; those are pure and ignored here.

(require racklr/uir
         "util.rkt")

(provide check-type
         type-check-ok?
         check-type-report
         unhandled-need unhandled-need?
         unhandled-need-component unhandled-need-capability
         unhandled-invocation unhandled-invocation?
         unhandled-invocation-component unhandled-invocation-capability unhandled-invocation-op
         missing-handler-op missing-handler-op?
         missing-handler-op-capability missing-handler-op-op
         missing-provided-op missing-provided-op?
         missing-provided-op-component missing-provided-op-capability missing-provided-op-op
         unhandled-handler-invocation unhandled-handler-invocation?
         unhandled-handler-invocation-handler unhandled-handler-invocation-capability unhandled-handler-invocation-op)

;; A capability a component asks for that no `given`/`via` chain satisfies.
(struct unhandled-need (component capability) #:transparent)

;; A capability operation a component body invokes that is not in its `needs`.
(struct unhandled-invocation (component capability op) #:transparent)

;; A capability operation that a `via` handler fails to implement.
(struct missing-handler-op (capability op) #:transparent)

;; A capability operation that a component's `provides` row fails to implement.
(struct missing-provided-op (component capability op) #:transparent)

;; A capability operation a handler body invokes that its own `via` row
;; does not cover. Mirrors unhandled-invocation at the handler layer.
(struct unhandled-handler-invocation (handler capability op) #:transparent)

;; Map each capability operation name to its owning capability's name.
(define (cap-op-owners decls)
  (for*/hash ([cap (in-list (filter uir-eff-capability? decls))]
              [op (in-list (uir-eff-capability-ops cap))])
    (values (uir-symbol-name (uir-eff-capability-op-name op))
            (uir-symbol-name (uir-eff-capability-name cap)))))

;; Map each capability name to the list of operation names it declares.
(define (cap-declared-ops decls)
  (for/hash ([cap (in-list (filter uir-eff-capability? decls))])
    (values (uir-symbol-name (uir-eff-capability-name cap))
            (map (λ (op) (uir-symbol-name (uir-eff-capability-op-name op)))
                 (uir-eff-capability-ops cap)))))

;; Every function/operation name an expression invokes, transitively.
(define (called-names e)
  (cond
    [(uir-call? e)
     (define callee (uir-call-callee e))
     (append (if (uir-symbol? callee) (list (uir-symbol-name callee)) '())
             (append-map called-names (uir-call-args e)))]
    [(uir-block? e) (append-map called-names (uir-block-stmts e))]
    [(uir-paren? e) (called-names (uir-paren-inner e))]
    [(uir-get? e) (called-names (uir-get-base e))]
    [(uir-set!? e) (called-names (uir-set!-value e))]
    [else '()]))

;; Capability ops a component body invokes that its `needs` row does not cover.
(define (component-invocation-violations c owners)
  (define needs-names
    (list->set (map (λ (r) (uir-symbol-name (uir-eff-ref-name r)))
                    (uir-eff-component-needs c))))
  (define cname (uir-symbol-name (uir-eff-component-name c)))
  (for*/list ([op (in-list (uir-eff-component-ops c))]
              [call (in-list (called-names (fourth op)))]
              #:when (hash-has-key? owners call)
              #:when (not (set-member? needs-names (hash-ref owners call))))
    (unhandled-invocation cname (hash-ref owners call) call)))

;; Every op a `via` handler's target capability declares that the handler omits.
(define (handler-completeness-violations h declared)
  (define cap-name (uir-symbol-name (uir-eff-ref-name (uir-eff-handler-effect h))))
  (define declared-ops (hash-ref declared cap-name '()))
  (define implemented
    (list->set (map (λ (op) (uir-symbol-name (first op)))
                    (uir-eff-handler-ops h))))
  (for/list ([op (in-list declared-ops)]
             #:unless (set-member? implemented op))
    (missing-handler-op cap-name op)))

;; Every op a component-provided capability declares that the component omits.
(define (component-provider-completeness-violations c declared)
  (define cname (uir-symbol-name (uir-eff-component-name c)))
  (define implemented
    (list->set (map (λ (op) (uir-symbol-name (first op)))
                    (uir-eff-component-ops c))))
  (for*/list ([p (in-list (uir-eff-component-provides c))]
              [op (in-list (hash-ref declared (uir-symbol-name (uir-eff-ref-name p)) '()))]
              #:unless (set-member? implemented op))
    (missing-provided-op cname (uir-symbol-name (uir-eff-ref-name p)) op)))

;; Capability ops a handler body invokes that its own `via` row does not cover.
(define (handler-invocation-violations h owners)
  (define row-names
    (list->set (map (λ (r) (uir-symbol-name (uir-eff-ref-name r)))
                    (uir-eff-handler-row h))))
  (define hname (uir-symbol-name (uir-eff-ref-name (uir-eff-handler-effect h))))
  (for*/list ([op (in-list (uir-eff-handler-ops h))]
              [call (in-list (called-names (fourth op)))]
              #:when (hash-has-key? owners call)
              #:when (not (set-member? row-names (hash-ref owners call))))
    (unhandled-handler-invocation hname (hash-ref owners call) call)))

(define (check-type sys)
  (define decls (uir-eff-system-decls sys))
  (define components (filter uir-eff-component? decls))
  (define handlers (filter uir-eff-handler? decls))
  (define given-keys
    (list->set
     (map ref-key
          (append-map uir-eff-given-row
                      (filter uir-eff-given? decls)))))
  (define via-edges
    (for/hash ([h (in-list handlers)])
      (values (ref-key (uir-eff-handler-effect h))
              (map ref-key (uir-eff-handler-row h)))))
  (define provider-needs
    (for/hash ([c (in-list components)])
      (values (uir-symbol-name (uir-eff-component-name c))
              (map ref-key (uir-eff-component-needs c)))))
  (define providers-by-key
    (for*/fold ([h (hash)])
               ([c (in-list components)]
                [p (in-list (uir-eff-component-provides c))])
      (hash-update h (ref-key p)
                   (λ (cs) (cons (uir-symbol-name (uir-eff-component-name c)) cs))
                   '())))
  (define (handled? key seen)
    (cond
      [(set-member? given-keys key) #t]
      [(set-member? seen key) #f]
      [else
       (define row (hash-ref via-edges key #f))
       (define via-ok (and row (pair? row)
                           (for/and ([k (in-list row)])
                             (handled? k (set-add seen key)))))
       (define provider-ok
         (for/or ([p (in-list (hash-ref providers-by-key key '()))])
           (for/and ([nk (in-list (hash-ref provider-needs p '()))])
             (handled? nk (set-add seen key)))))
       (or via-ok provider-ok)]))
  (append
   (for*/list ([c (in-list components)]
               [n (in-list (uir-eff-component-needs c))]
               #:unless (handled? (ref-key n) (set)))
     (unhandled-need (uir-symbol-name (uir-eff-component-name c)) n))
   (append-map (λ (c) (component-invocation-violations c (cap-op-owners decls)))
               components)
   (append-map (λ (h) (handler-completeness-violations h (cap-declared-ops decls)))
               handlers)
   (append-map (λ (c) (component-provider-completeness-violations c (cap-declared-ops decls)))
               components)
   (append-map (λ (h) (handler-invocation-violations h (cap-op-owners decls)))
               handlers)))

(define (type-check-ok? sys)
  (null? (check-type sys)))

(define (check-type-report issues)
  (for/list ([u (in-list issues)])
    (cond
      [(unhandled-need? u)
       (format "unhandled capability: component ~a needs ~a"
               (unhandled-need-component u)
               (ref-text (unhandled-need-capability u)))]
      [(unhandled-invocation? u)
       (format "unhandled invocation: component ~a calls ~a (capability ~a) outside its needs"
               (unhandled-invocation-component u)
               (unhandled-invocation-op u)
               (unhandled-invocation-capability u))]
      [(missing-handler-op? u)
       (format "incomplete handler: ~a via ... does not implement ~a"
               (missing-handler-op-capability u)
               (missing-handler-op-op u))]
      [(missing-provided-op? u)
       (format "incomplete provider: component ~a provides ~a but does not implement ~a"
               (missing-provided-op-component u)
               (missing-provided-op-capability u)
               (missing-provided-op-op u))]
      [(unhandled-handler-invocation? u)
       (format "unhandled handler invocation: ~a via ... calls ~a (capability ~a) outside its row"
               (unhandled-handler-invocation-handler u)
               (unhandled-handler-invocation-op u)
               (unhandled-handler-invocation-capability u))]
      [else "?"])))

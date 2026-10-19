#lang racket

;; Effect-row reachability: is every capability a component asks for backed
;; by a `given` capability, possibly through `via` interpretations?
;;
;; A need is satisfied when:
;;   - it names a `given` capability directly, or
;;   - it names a capability with a `via` interpretation whose own row is
;;     satisfied (recursively).
;;
;; This is pure graph reachability over UIR: no runtime semantics, no
;; expression evaluation.

(require racklr/uir
         "util.rkt")

(provide check-type
         type-check-ok?
         check-type-report
         unhandled-need unhandled-need?
         unhandled-need-component unhandled-need-capability)

;; A capability a component asks for that no `given`/`via` chain satisfies.
(struct unhandled-need (component capability) #:transparent)

(define (check-type sys)
  (define decls (uir-eff-system-decls sys))
  (define given-keys
    (list->set
     (map ref-key
          (append-map uir-eff-given-row
                      (filter uir-eff-given? decls)))))
  (define via-edges
    (for/hash ([h (in-list (filter uir-eff-handler? decls))])
      (values (ref-key (uir-eff-handler-effect h))
              (map ref-key (uir-eff-handler-row h)))))
  (define (handled? key seen)
    (cond
      [(set-member? given-keys key) #t]
      [(set-member? seen key) #f]
      [else
       (define row (hash-ref via-edges key #f))
       (and row
            (pair? row)
            (for/and ([k (in-list row)])
              (handled? k (set-add seen key))))]))
  (for*/list ([c (in-list (filter uir-eff-component? decls))]
              [n (in-list (uir-eff-component-needs c))]
              #:unless (handled? (ref-key n) (set)))
    (unhandled-need (uir-symbol-name (uir-eff-component-name c)) n)))

(define (type-check-ok? sys)
  (null? (check-type sys)))

(define (check-type-report issues)
  (for/list ([u (in-list issues)])
    (format "unhandled capability: component ~a needs ~a"
            (unhandled-need-component u)
            (ref-text (unhandled-need-capability u)))))

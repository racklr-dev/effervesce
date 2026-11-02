#lang racket

;; Bridge checker: connect the declarative Effervesce spec to inferred effect
;; rows of real host code.
;;
;; For each `component C needs R`, the checker maps R through a
;; capability-interpretation table (capability name -> allowed host effect
;; names), infers the effects actually performed by C's host code, and reports
;; every inferred effect C's needs do not cover. Each violation carries the
;; classifying call sites (provenance, docs/effects-proposal.md §6.1): which
;; function performed the effect and through what callee expression.
;;
;; Unresolvable calls (higher-order) surface as 'unknown in the inferred row
;; and are reported as `bridge-unverified` rather than a hard violation.

(require racklr/uir
         racklr/effects/infer
         racklr/effects/contract)

(provide check-bridge
         bridge-ok?
         bridge-violation bridge-violation?
         bridge-violation-component bridge-violation-effect bridge-violation-sites
         bridge-unverified bridge-unverified?
         bridge-unverified-component)

(struct bridge-violation (component effect sites) #:transparent)
(struct bridge-unverified (component) #:transparent)

;; All effect-name symbols inferred anywhere in a program's functions.
(define (program-effect-names rows)
  (remove-duplicates
   (append-map
    (λ (p) (map effect-name-symbol
                (uir-effect-row-entries p)))
    (hash-values rows))))

;; Capability names a component's needs cover, expanded through the
;; interpretation table to the set of allowed host effect names.
(define (allowed-effect-names needs interpretations)
  (list->set
   (append-map
    (λ (ref)
      (hash-ref interpretations
                 (uir-symbol-name (uir-eff-ref-name ref))
                 '()))
    needs)))

;; Classifying call sites (function:callee) that produced effect `eff` across a
;; component's provenance table (name → (hash effect → (listof site))).
(define (effect-sites prov eff)
  (sort
   (for*/list ([(nm h) (in-hash prov)]
               [s (in-list (hash-ref h eff '()))])
     (format "~a: ~a" nm s))
   string<?))

(define (check-bridge spec host-code interpretations)
  (define components
    (filter uir-eff-component? (uir-eff-system-decls spec)))
  (define issues '())
  (for ([c (in-list components)])
    (define cname (uir-symbol-name (uir-eff-component-name c)))
    (define program (hash-ref host-code cname #f))
    (if (not program)
        (void)
        (let* ([allowed (allowed-effect-names (uir-eff-component-needs c)
                                              interpretations)]
               [prov (box (make-hash))]
               [rows (infer-effects program #:provenance-table prov)]
               [used (program-effect-names rows)])
          (for ([e (in-list used)])
            (cond
              [(eq? e 'unknown)
               (set! issues (cons (bridge-unverified cname) issues))]
              [(not (set-member? allowed e))
               (set! issues (cons (bridge-violation cname e
                                                   (effect-sites (unbox prov) e))
                                  issues))]
              [else (void)])))))
  (reverse issues))

(define (bridge-ok? issues)
  (null? (filter bridge-violation? issues)))

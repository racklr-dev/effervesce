#lang racket

;; Bridge checker: connect the declarative Effervesce spec to inferred effect
;; rows of real host code.
;;
;; For each `component C needs R`, the checker resolves R through the
;; capability-interpretation table into an allowed effect row and checks the
;; inferred row of C's host code against it with the full effect algebra
;; (`effect-subtype?`, contract.rkt §3.3): an inferred entry is covered when
;; the capability row carries the same effect with a permitting modality, a
;; budget at least as large, and ops at least as broad. This is Phase 1.4 —
;; the table's value is a row, not a flat name set; a flat list of names is
;; still accepted as the degenerate "all ops, unbounded" form.
;;
;; Each violation carries the classifying call sites (provenance,
;; docs/effects-proposal.md §6.1): which function performed the effect and
;; through what callee expression.
;;
;; Higher-order (bare callback) calls surface as `dynamic` in the inferred
;; row and are reported as a hard violation unless the component's needs
;; cover `dynamic`; member calls on unresolved receivers surface as 'unknown
;; and are reported as `bridge-unverified` rather than a hard violation.

(require racklr/uir
         racklr/effects/infer
         racklr/effects/contract
         racklr/effects/algebra)

(provide check-bridge
         bridge-ok?
         bridge-violation bridge-violation?
         bridge-violation-component bridge-violation-effect bridge-violation-sites
         bridge-unverified bridge-unverified?
         bridge-unverified-component)

(struct bridge-violation (component effect sites) #:transparent)
(struct bridge-unverified (component) #:transparent)

;; One interpretation-table value → an effect row. Accepted forms:
;;   - uir-effect-row             → used as-is (full algebra: ops/budget/modality)
;;   - (listof uir-effect-entry)  → a row of those entries
;;   - (listof symbol)            → degenerate: each name is a 'may entry with
;;                                  unbounded ops/budget (the legacy flat form)
(define (interpretation->row v)
  (cond
    [(uir-effect-row? v) v]
    [(and (pair? v) (uir-effect-entry? (car v))) (uir-effect-row v)]
    [(pair? v)
     (uir-effect-row
      (for/list ([s (in-list v)])
        (uir-effect-entry (uir-symbol (symbol->string s)) 'may '() #f #f)))]
    [else (empty-row)]))

;; A component's `needs` resolved through the interpretation table to a single
;; allowed row: the algebra join of each referenced capability's row.
(define (allowed-row needs interpretations)
  (foldl effect-join (empty-row)
         (for/list ([ref (in-list needs)])
           (interpretation->row
            (hash-ref interpretations
                       (uir-symbol-name (uir-eff-ref-name ref))
                       #f)))))

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
    (when program
      (let* ([allowed (allowed-row (uir-eff-component-needs c) interpretations)]
             [prov (box (make-hash))]
             [rows (infer-effects program #:provenance-table prov)]
             [inferred (foldl effect-join (empty-row) (hash-values rows))])
        (for ([e (in-list (uir-effect-row-entries inferred))])
          (define n (effect-name-symbol e))
          (cond
            [(eq? n 'unknown)
             (set! issues (cons (bridge-unverified cname) issues))]
            [(not (effect-subtype? (uir-effect-row (list e)) allowed))
             (set! issues (cons (bridge-violation cname n
                                                 (effect-sites (unbox prov) n))
                                issues))]
            [else (void)])))))
  (reverse issues))

(define (bridge-ok? issues)
  (null? (filter bridge-violation? issues)))

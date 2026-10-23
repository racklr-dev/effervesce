#lang racket

;; Policy checking: evaluate each `never` / `must` predicate over the
;; description. A predicate is a structural `uir-call` tree (`component-needs`,
;; `not`, `and`, `or`), so this is a pure walk — no runtime semantics.

(require racklr/uir
         "util.rkt")

(provide check-policy
         policy-check-ok?
         check-policy-report
         policy-violation policy-violation?
         policy-violation-kind policy-violation-predicate)

;; A policy rule that fails: kind is 'never | 'must, predicate is the uir expr.
(struct policy-violation (kind predicate) #:transparent)

(define (system-components sys)
  (filter uir-eff-component? (uir-eff-system-decls sys)))

;; `component needs X`: does any component ask for the exact capability X?
;; Full ref identity (name + type args), matching the type checker.
(define (component-needs? sys ref)
  (define key (ref-key ref))
  (for*/or ([c (in-list (system-components sys))]
            [n (in-list (uir-eff-component-needs c))])
    (equal? key (ref-key n))))

(define (eval-pred p sys)
  (cond
    [(uir-paren? p) (eval-pred (uir-paren-inner p) sys)]
    [(uir-call? p)
     (define op (uir-symbol-name (uir-call-callee p)))
     (define args (uir-call-args p))
     (define a (and (pair? args) (eval-pred (first args) sys)))
     (define b (and (pair? args) (pair? (cdr args)) (eval-pred (second args) sys)))
     (cond
       [(equal? op "component-needs") (component-needs? sys (first args))]
       [(equal? op "not") (not a)]
       [(equal? op "and") (and a b)]
       [(equal? op "or") (or a b)]
       [else #f])]
    [else #f]))

(define (check-policy sys)
  (for/list ([p (in-list (filter uir-eff-policy? (uir-eff-system-decls sys)))]
             #:when (let ([holds? (eval-pred (uir-eff-policy-predicate p) sys)])
                      (cond
                        [(eq? (uir-eff-policy-kind p) 'never) holds?]
                        [(eq? (uir-eff-policy-kind p) 'must) (not holds?)]
                        [else #f])))
    (policy-violation (uir-eff-policy-kind p) (uir-eff-policy-predicate p))))

(define (policy-check-ok? sys)
  (null? (check-policy sys)))

(define (pred-text p)
  (cond
    [(uir-paren? p) (format "(~a)" (pred-text (uir-paren-inner p)))]
    [(uir-call? p)
     (define op (uir-symbol-name (uir-call-callee p)))
     (define args (uir-call-args p))
     (cond
       [(equal? op "component-needs") (format "component needs ~a" (ref-text (first args)))]
       [(equal? op "not") (format "not (~a)" (pred-text (first args)))]
       [(equal? op "and") (format "(~a and ~a)" (pred-text (first args)) (pred-text (second args)))]
       [(equal? op "or") (format "(~a or ~a)" (pred-text (first args)) (pred-text (second args)))]
       [else op])]
    [else "?"]))

(define (check-policy-report issues)
  (for/list ([v (in-list issues)])
    (format "policy violation: ~a rule fails (~a)"
            (policy-violation-kind v)
            (pred-text (policy-violation-predicate v)))))

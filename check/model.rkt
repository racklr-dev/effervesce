#lang racket

;; Model checking: static state-safety plus bounded reachable-state
;; exploration of `:=` transitions.
;;
;; An `always` invariant is checked over every reachable state:
;;   - any state where it evaluates to #f        -> invariant-violated (hard)
;;   - all states evaluate to a non-unknown value -> discharged (no finding)
;;   - some state is 'unknown (uninterpreted op) -> invariant-unverified (note)
;;   - exploration hit its bound before closure  -> invariant-unverified (note)
;;
;; Capability operations stay abstract: their results are 'unknown, so an
;; invariant over them is reported unverified rather than silently passed.

(require racklr/uir)

(provide check-model
         model-check-ok?
         model-issues-ok?
         check-model-report
         model-issue model-issue?
         model-issue-kind model-issue-message)

;; kind is one of:
;;   'duplicate-state          (hard)
;;   'unknown-state-write      (hard)
;;   'unknown-state-reference  (hard)
;;   'invariant-violated       (hard)
;;   'invariant-unverified     (informational)
(struct model-issue (kind message) #:transparent)

(define hard-kinds '(duplicate-state unknown-state-write unknown-state-reference invariant-violated))

(define max-depth 20)
(define max-states 10000)

(define (duplicates xs)
  (remove-duplicates
   (for/list ([x (in-list xs)]
              #:when (member x (cdr (member x xs))))
     x)))

;; Free variable names an expression reads (var refs and field-access bases).
(define (collect-free-vars e)
  (cond
    [(uir-var? e) (list (uir-symbol-name (uir-var-name e)))]
    [(uir-get? e) (collect-free-vars (uir-get-base e))]
    [(uir-paren? e) (collect-free-vars (uir-paren-inner e))]
    [(uir-call? e) (append-map collect-free-vars (uir-call-args e))]
    [(uir-block? e) (append-map collect-free-vars (uir-block-stmts e))]
    [(uir-set!? e) (collect-free-vars (uir-set!-value e))]
    [else '()]))

;; Evaluator over a state environment (name -> value | 'unknown).
;; Uninterpreted calls, undeclared reads, and field access are 'unknown.
(define (eval-expr e env)
  (cond
    [(uir-bool? e) (uir-bool-value e)]
    [(uir-number? e) (or (string->number (uir-number-value e)) 'unknown)]
    [(uir-string? e) (uir-string-value e)]
    [(uir-null? e) 'none]
    [(uir-var? e) (hash-ref env (uir-symbol-name (uir-var-name e)) 'unknown)]
    [(uir-paren? e) (eval-expr (uir-paren-inner e) env)]
    [(uir-call? e)
     (define op (uir-symbol-name (uir-call-callee e)))
     (define args (uir-call-args e))
     (define a (and (pair? args) (eval-expr (first args) env)))
     (define b (and (pair? args) (pair? (cdr args)) (eval-expr (second args) env)))
     (define (unk? x) (eq? x 'unknown))
     (cond
       [(equal? op "not") (if (unk? a) 'unknown (not a))]
       [(equal? op "and")
        (cond [(and (unk? a) (unk? b)) 'unknown]
              [(unk? a) (if b b 'unknown)]
              [(unk? b) (if a a 'unknown)]
              [else (and a b)])]
       [(equal? op "or")
        (cond [(and (unk? a) (unk? b)) 'unknown]
              [(unk? a) (if b #t 'unknown)]
              [(unk? b) (if a #t 'unknown)]
              [else (or a b)])]
       [(equal? op "==") (if (or (unk? a) (unk? b)) 'unknown (equal? a b))]
       [(equal? op "!=") (if (or (unk? a) (unk? b)) 'unknown (not (equal? a b)))]
       [(member op '("<" "<=" ">" ">="))
        (if (or (unk? a) (unk? b) (not (number? a)) (not (number? b)))
            'unknown
            (case (string->symbol op)
              [(<) (< a b)] [(<=) (<= a b)] [(>) (> a b)] [(>=) (>= a b)]))]
        [(member op '("+" "-" "*" "/" "%"))
         (cond
           [(and (equal? op "+") (string? a) (string? b)) (string-append a b)]
           [(or (unk? a) (unk? b) (not (number? a)) (not (number? b))) 'unknown]
           [else
            (case (string->symbol op)
              [(+) (+ a b)] [(-) (- a b)] [(*) (* a b)]
              [(/) (if (zero? b) 'unknown (/ a b))]
              [(%) (if (zero? b) 'unknown (modulo a b))])])]
       [else 'unknown])]
    [else 'unknown]))

;; Apply one action's `:=` steps to a state, returning the successor env.
(define (apply-action action env)
  (for/fold ([e env])
            ([step (in-list (uir-eff-action-steps action))])
    (if (uir-set!? step)
        (hash-set e
                  (uir-symbol-name (uir-set!-name step))
                  (eval-expr (uir-set!-value step) e))
        e)))

;; All states reachable from the initial state by applying actions.
;; Returns (values envs truncated?) where envs is a list and truncated? marks
;; that exploration stopped at a bound before closure.
(define (reachable-states initial-env actions)
  (define seen (set))
  (define acc '())
  (define truncated? #f)
  (let loop ([frontier (list initial-env)] [depth 0])
    (cond
      [(null? frontier) (values (reverse acc) truncated?)]
      [(or (>= depth max-depth) (>= (length acc) max-states))
       (set! truncated? #t)
       (values (reverse acc) truncated?)]
      [else
       (for ([env (in-list frontier)]
             #:unless (set-member? seen env))
         (set! seen (set-add seen env))
         (set! acc (cons env acc)))
       (define nexts
         (for*/list ([env (in-list frontier)]
                     [a (in-list actions)]
                     #:unless (set-member? seen (apply-action a env)))
           (apply-action a env)))
       (loop (remove-duplicates nexts) (+ depth 1))])))

(define (check-model sys)
  (define decls (uir-eff-system-decls sys))
  (define states (filter uir-eff-state? decls))
  (define state-names (map (λ (s) (uir-symbol-name (uir-eff-state-name s))) states))
  (define state-name-set (list->set state-names))
  (define actions (filter uir-eff-action? decls))
  (define invs (filter uir-eff-invariant? decls))
  (define issues '())
  (for ([n (in-list (duplicates state-names))])
    (set! issues (cons (model-issue 'duplicate-state
                                    (format "state ~a declared more than once" n))
                       issues)))
  (for* ([a (in-list actions)]
         [step (in-list (uir-eff-action-steps a))]
         #:when (uir-set!? step))
    (define t (uir-symbol-name (uir-set!-name step)))
    (unless (set-member? state-name-set t)
      (set! issues (cons (model-issue 'unknown-state-write
                                      (format "action ~a writes undeclared state ~a"
                                              (uir-symbol-name (uir-eff-action-name a)) t))
                         issues))))
  (define initial-env
    (for/hash ([s (in-list states)])
      (values (uir-symbol-name (uir-eff-state-name s))
              (eval-expr (uir-eff-state-init s) (hash)))))
  (define-values (reachable truncated?)
    (reachable-states initial-env actions))
  (for ([inv (in-list invs)])
    (define e (uir-eff-invariant-expr inv))
    (for ([v (in-list (collect-free-vars e))]
          #:unless (set-member? state-name-set v))
      (set! issues (cons (model-issue 'unknown-state-reference
                                      (format "invariant references undeclared state ~a" v))
                         issues)))
    (define results (map (λ (env) (eval-expr e env)) reachable))
    (cond
      [(for/or ([r (in-list results)]) (eq? r #f))
       (set! issues (cons (model-issue 'invariant-violated
                                       "invariant fails in a reachable state")
                          issues))]
      [(or truncated? (for/or ([r (in-list results)]) (eq? r 'unknown)))
       (set! issues (cons (model-issue 'invariant-unverified
                                       "invariant unverified (uninterpreted operations, field access, or bounded exploration)")
                          issues))]
      [else (void)]))
  (reverse issues))

(define (model-issues-ok? issues)
  (not (for/or ([i (in-list issues)])
         (member (model-issue-kind i) hard-kinds))))

(define (model-check-ok? sys)
  (model-issues-ok? (check-model sys)))

(define (check-model-report issues)
  (for/list ([i (in-list issues)])
    (format "~a: ~a" (model-issue-kind i) (model-issue-message i))))

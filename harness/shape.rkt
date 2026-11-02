#lang racket

;; effervesce/harness/shape.rkt
;;
;; P5.3 — Loop as data: shape extraction.
;;
;; The loop shape is read out of the manifest's driver UIR, never out of Racket
;; control flow. `loop-shape` walks the `uir-fn` body that the manifest names as
;; its driver and returns a plain datum describing the stages and control flow:
;;
;;   (loop (seq (step "observe")
;;              (step "tick")
;;              (while (< round 3)
;;                     (seq (step "decide") (step "act") ...))))
;;
;; A manifest rewrite that changes the driver datum (reorder stages, move a
;; stage inside the loop, change the bound) changes this shape without touching
;; any Racket code.

(require racklr/uir
         racket/set)

(provide loop-shape)

;; ── names ───────────────────────────────────────────────────────────

(define (program-fns program)
  (filter uir-fn? (uir-block-stmts program)))

(define (fn-name fn)
  (define n (uir-fn-name fn))
  (and n (uir-symbol-name n)))

(define (name-of node)
  (cond [(uir-var? node) (uir-symbol-name (uir-var-name node))]
        [(uir-symbol? node) (uir-symbol-name node)]
        [else #f]))

;; The dotted spelling of a receiver chain, e.g. (get (var "time") (symbol "time"))
;; -> "time.time". Returns #f for anything that is not a chain of names.
(define (dotted-name node)
  (cond
    [(or (uir-var? node) (uir-symbol? node)) (name-of node)]
    [(uir-get? node)
     (define base (dotted-name (uir-get-base node)))
     (define field (name-of (uir-get-field node)))
     (and base field (string-append base "." field))]
    [else #f]))

(define primitive-names
  (set "<" ">" "<=" ">=" "=" "!=" "+" "-" "*" "/" "not" "and" "or"))

;; ── shape ───────────────────────────────────────────────────────────

(define (shape-of node steps)
  (cond
    [(uir-null? node) #f]
    [(uir-number? node)
     (or (string->number (uir-number-value node)) (uir-number-value node))]
    [(uir-bool? node) (uir-bool-value node)]
    [(uir-string? node) (uir-string-value node)]
    [(or (uir-var? node) (uir-symbol? node)) (string->symbol (name-of node))]
    [(uir-block? node)
     (cons 'seq (filter values (map (lambda (s) (shape-of s steps))
                                    (uir-block-stmts node))))]
    [(uir-call? node)
     (define callee (uir-call-callee node))
     (define callee-name (name-of callee))
     (define arg-shapes (map (lambda (a) (shape-of a steps)) (uir-call-args node)))
     (cond
       [(and callee-name (set-member? steps callee-name))
        (list 'step callee-name)]
       [(and callee-name (set-member? primitive-names callee-name))
        (cons (string->symbol callee-name) arg-shapes)]
       [(dotted-name callee)
        => (lambda (dn) (list 'op dn))]
       [else (list 'call (or callee-name (uir-tag callee)))])]
    [(uir-while? node)
     (list 'while
           (shape-of (uir-while-test node) steps)
           (shape-of (uir-while-body node) steps))]
    [(uir-if? node)
     (list 'if
           (shape-of (uir-if-test node) steps)
           (shape-of (uir-if-then node) steps)
           (shape-of (uir-if-else node) steps))]
    [(uir-set!? node)
     (list 'set!
           (uir-symbol-name (uir-set!-name node))
           (shape-of (uir-set!-value node) steps))]
    [(uir-let? node)
     (list 'let
           (uir-symbol-name (uir-let-name node))
           (shape-of (uir-let-body node) steps))]
    [(uir-return? node)
     (list 'return (shape-of (uir-return-value node) steps))]
    [else (list 'expr (uir-tag node))]))

;; ── entry point ─────────────────────────────────────────────────────

(define (loop-shape program driver-name)
  (unless (uir-block? program)
    (error 'loop-shape "manifest program is not a uir-block: ~e" program))
  (define fns (program-fns program))
  (define step-names
    (for/set ([n (in-list (map fn-name fns))]
              #:when (and n (not (equal? n driver-name))))
      n))
  (define driver
    (for/or ([fn (in-list fns)])
      (and (equal? (fn-name fn) driver-name) fn)))
  (unless driver
    (error 'loop-shape "manifest program has no driver function named ~a" driver-name))
  (list 'loop (shape-of (uir-fn-body driver) step-names)))

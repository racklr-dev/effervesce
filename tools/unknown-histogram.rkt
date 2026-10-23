#lang racket

;; Call-shape census of every `uir-call` that the name-heuristic tail of the
;; effect classifier leaves `unknown` (i.e. NOT an intra-program edge and NOT a
;; recognized pure/effect root). This is the diagnostic that drives the next
;; `unknown`-reduction lever: it reports the dominant callee shapes so a new
;; root table or resolution rule can be aimed at real call sites instead of
;; guessed from the aggregate count.
;;
;; Usage:
;;   racket tools/unknown-histogram.rkt <dir> [--top N]
;;
;; Note: this classifies with empty known-names, so it reports the NON-EDGE
;; classification only. Bare calls to functions/methods defined in the tree are
;; separated out (they resolve as edges in real inference) so the residual is
;; the genuinely unresolvable surface.

(require racklr/uir
         racklr/effects/infer
         "../check/host-python.rkt")

(define (find-py-files dir)
  (define out '())
  (let loop ([d dir])
    (for ([e (in-list (directory-list d))])
      (define p (build-path d e))
      (cond
        [(directory-exists? p) (loop p)]
        [(regexp-match? #rx"[.]py$" e) (set! out (cons p out))]
        [else (void)])))
  (reverse out))

;; Leftmost root of a dotted callee (mirror of infer.rkt's dotted-root).
(define (symbol-root name)
  (let loop ([i 0])
    (cond [(>= i (string-length name)) (string->symbol name)]
          [(char=? (string-ref name i) #\/) (string->symbol (substring name 0 i))]
          [else (loop (add1 i))])))

(define (dotted-root c)
  (let loop ([c c])
    (cond
      [(uir-get? c) (loop (uir-get-base c))]
      [(uir-var? c) (symbol-root (uir-symbol-name (uir-var-name c)))]
      [(uir-symbol? c) (symbol-root (uir-symbol-name c))]
      [else #f])))

;; Full structural walk (visits every struct node, including nested fns).
(define (walk-all n visit!)
  (cond
    [(struct? n)
     (visit! n)
     (for ([f (in-list (cdr (vector->list (struct->vector n))))])
       (walk-all f visit!))]
    [(pair? n) (walk-all (car n) visit!) (walk-all (cdr n) visit!)]
    [else (void)]))

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define dir (and (pair? args) (car args)))
  (define top-n
    (or (for/or ([a (in-list args)] [i (in-naturals)]
                #:when (string=? a "--top"))
          (and (< (add1 i) (length args))
               (string->number (list-ref args (add1 i)))))
        30))
  (unless dir
    (eprintf "usage: unknown-histogram.rkt <dir> [--top N]\n")
    (exit 1))

  (define files (sort (find-py-files dir)
                      (λ (a b) (string<? (path->string a) (path->string b)))))
  (define host-code (make-hash))
  (for ([f (in-list files)])
    (define uir (python-source->uir (file->string f)))
    (when uir (hash-set! host-code (path->string f) uir)))

  ;; Names defined anywhere in the tree: bare fn names + Class.method names.
  ;; A bare call to one of these resolves as an edge in whole-program inference,
  ;; so it is not a genuine `unknown`.
  (define known (make-hash))
  (for ([(c p) (in-hash host-code)])
    (for ([(n _) (in-hash (collect-functions p))]) (hash-set! known n #t))
    (for ([(n _) (in-hash (collect-methods p))]) (hash-set! known n #t)))

  (define counts (make-hash))
  (define total 0)
  (define (bump key)
    (hash-update! counts key add1 0)
    (set! total (add1 total)))

  (for ([(c p) (in-hash host-code)])
    (walk-all
     p
     (λ (n)
       (when (uir-call? n)
         (define callee (uir-call-callee n))
         (define cls (classify-call callee (hash)))
         (when (eq? (car cls) 'unknown)
           (define op (effect-op-of-callee callee))
           (define root (dotted-root callee))
           (define nm
             (cond
               [(uir-var? callee) (uir-symbol-name (uir-var-name callee))]
               [(uir-symbol? callee) (uir-symbol-name callee)]
               [else #f]))
           (cond
             [(uir-get? callee)
              (bump (string->symbol
                     (format "dotted ~a.~a" (or root "?") (or op "?"))))]
             [(and nm (hash-has-key? known (string->symbol nm)))
              (bump (string->symbol (format "edge-known ~a" nm)))]
             [else
              (bump (string->symbol (format "bare ~a" (or nm "?"))))]))))))

  (printf "unknown (non-edge) call sites: ~a\n" total)
  (define ranked
    (sort (hash->list counts)
          (λ (a b) (> (cdr a) (cdr b)))))
  (for ([e (in-list (take ranked (min top-n (length ranked))))])
    (printf "  ~a  x~a\n" (car e) (cdr e))))

(main)

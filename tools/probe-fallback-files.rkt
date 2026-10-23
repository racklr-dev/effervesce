#lang racket
;; One-off probe: which Django files carry each remaining fallback symbol.
(require racket/list
         racklr/uir
         racklr/lower-python
         "../check/host-python.rkt")

(define (fallback-symbols n acc)
  (cond
    [(uir-symbol? n)
     (define nm (uir-symbol-name n))
     (if (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\?))
         (cons nm acc) acc)]
    [(struct? n)
     (for/fold ([a acc]) ([f (in-list (cdr (vector->list (struct->vector n))))])
       (fallback-symbols f a))]
    [(pair? n)
     (fallback-symbols (cdr n) (fallback-symbols (car n) acc))]
    [else acc]))

(define (find-py-files dir)
  (define out '())
  (let loop ([d dir])
    (for ([e (in-list (directory-list d))])
      (define p (build-path d e))
      (cond [(directory-exists? p) (loop p)]
            [(regexp-match? #rx"[.]py$" e) (set! out (cons p out))]
            [else (void)])))
  (reverse out))

(define root (vector-ref (current-command-line-arguments) 0))
(define wanted (cdr (vector->list (current-command-line-arguments))))

(for ([p (in-list (find-py-files root))])
  (define src (file->string p))
  (define uir (python-source->uir src))
  (when uir
    (define syms (remove-duplicates (fallback-symbols uir '())))
    (define hits (filter (λ (s) (ormap (λ (w) (string-prefix? s w)) wanted)) syms))
    (when (pair? hits)
      (printf "~a  ~a\n" p (string-join hits " ")))))

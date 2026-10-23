#lang racket
;; Measure the distinct `?`-prefixed fallback symbols and bare-`?` fields across a
;; Python tree, to confirm the class-docstring fix and rank the remaining gaps.
;; Usage: racket tools/measure-fallback.rkt ../django/django

(require racket/list
         racklr/uir
         racklr/lower-python
         "../check/host-python.rkt")

(define (fallback-symbols n acc)
  (cond
    [(uir-symbol? n)
     (define nm (uir-symbol-name n))
     (if (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\?))
         (cons nm acc)
         acc)]
    [(struct? n)
     (for/fold ([a acc]) ([f (in-list (cdr (vector->list (struct->vector n))))])
       (fallback-symbols f a))]
    [(pair? n)
     (fallback-symbols (cdr n) (fallback-symbols (car n) acc))]
    [else acc]))

(define (bare-?-field? uir)
  ;; true if the UIR contains a uir-field whose name is the symbol "?"
  (cond
    [(uir-field? uir)
     (equal? (uir-symbol-name (uir-field-name uir)) "?")]
    [(struct? uir)
     (for/or ([f (in-list (cdr (vector->list (struct->vector uir))))])
       (bare-?-field? f))]
    [(pair? uir)
     (or (bare-?-field? (car uir)) (bare-?-field? (cdr uir)))]
    [else #f]))

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

(define root (vector-ref (current-command-line-arguments) 0))
(define py-files (find-py-files root))

(define counts (make-hash))
(define bare-? 0)
(define fallback-files 0)

(for ([p (in-list py-files)])
  (define src (file->string p))
  (define uir (python-source->uir src))
  (when uir
    (define fbs (remove-duplicates (fallback-symbols uir '())))
    (when (pair? fbs)
      (set! fallback-files (+ fallback-files 1))
      (for ([s (in-list fbs)])
        (hash-update! counts s add1 0)))
    (when (bare-?-field? uir)
      (set! bare-? (+ bare-? 1)))))

(printf "files with any fallback symbol: ~a\n" fallback-files)
(printf "files with a bare '?' field:     ~a\n" bare-?)
(printf "distinct fallback symbols:\n")
(for ([(s n) (in-hash counts)])
  (printf "  ~a  ~a\n" n s))

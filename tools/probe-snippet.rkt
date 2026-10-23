#lang racket
;; Quick single-snippet probe: parse+lower a Python snippet and print the UIR
;; as an s-expression, to verify slice / star / underscore-var lowering.
(require racklr/uir
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

(for ([src (in-list (vector->list (current-command-line-arguments)))])
  (printf "SRC: ~a\n" src)
  (define uir (python-source->uir src))
  (if uir
      (begin
        (printf "UIR: ~a\n" (uir->sexp uir))
        (printf "FALLBACK: ~a\n" (remove-duplicates (fallback-symbols uir '()))))
      (printf "PARSE-FAIL\n"))
  (printf "\n"))

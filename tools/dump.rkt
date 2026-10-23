#lang racket
(require racklr/uir racklr/lower-python "../check/host-python.rkt")
(define u (python-source->uir (file->string (vector-ref (current-command-line-arguments) 0))))
(define (show n depth)
  (define pad (make-string (* depth 2) #\space))
  (cond
    [(uir-symbol? n) (printf "~asym ~s\n" pad (uir-symbol-name n))]
    [(uir-field? n) (printf "~aFIELD name=" pad) (show (uir-field-name n) 0) (printf "~a  init=" pad) (show (uir-field-init n) (add1 depth))]
    [(uir-class? n)
     (printf "~aCLASS name=" pad) (show (uir-class-name n) 0)
     (for ([f (in-list (uir-class-fields n))]) (show f (add1 depth)))]
    [(struct? n) (for ([f (in-list (cdr (vector->list (struct->vector n))))]) (show f depth))]
    [(pair? n) (show (car n) depth) (show (cdr n) depth)]
    [else (printf "~a<~a>\n" pad n)]))
(show u 0)

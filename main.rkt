#lang racket

(require "check.rkt")

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (match args
    [(list path)
     (define v (check-file path))
     (define report (check-report v))
     (for ([m (in-list report)])
       (printf "  ~a\n" m))
     (printf "~a: ~a\n" path (if (check-ok? v) "ok" "failed"))
     (exit (if (check-ok? v) 0 1))]
    [_ (eprintf "usage: racket main.rkt <file.fizz>\n") (exit 2)]))

(module+ main
  (main))

#lang racket

(require "check.rkt"
         "load.rkt"
         "check/graph.rkt"
         "check/query.rkt"
         "check/verify.rkt"
         racket/file)

(define (render q a)
  (define v (answer-value a))
  (match q
    [(list 'capabilities-of _)
     (if (null? v) "(none)" (string-join (map format-ref-key v) ", "))]
    [(list 'reachable? _) (if v "yes" "no")]
    [(list (or 'effects-of 'side-effects-of 'touches 'resources-of) _)
     (if (null? v) "(none)" (string-join (map symbol->string v) ", "))]
    [_ (format "~a" v)]))

(define (run-ask spec-path question)
  (define sys (load-effervesce (file->string spec-path)))
  (define idx (make-index (system-graph sys)))
  (define q
    (match question
      [(list "capabilities-of" a) (list 'capabilities-of a)]
      [(list "reachable?" a) (list 'reachable? (cons a '()))]
      [(list (and k (or "effects-of" "side-effects-of" "touches" "resources-of")) a)
       (list (string->symbol k) a)]
      [other (map string->symbol other)]))
  (cond
    [(host-level-question? q)
     (printf "unknown [~a: host artifact required, not present in a bare .fizz spec]\n"
             (car q))]
    [else
     (define a (ask idx q))
     (printf "~a [~a]\n" (render q a) (answer-confidence a))]))

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (match args
    [(list "ask" spec-path q ...)
     (run-ask spec-path q)]
    [(list "verify" spec-path)
     (define sys (load-effervesce (file->string spec-path)))
     (define fs (verify sys (hash) (hash)))
     (for ([m (in-list (verify-report fs))])
       (printf "  ~a\n" m))
     (printf "~a: ~a\n" spec-path (if (verify-ok? fs) "ok" "failed"))
     (exit (if (verify-ok? fs) 0 1))]
    [(list path)
     (define v (check-file path))
     (define report (check-report v))
     (for ([m (in-list report)])
       (printf "  ~a\n" m))
     (printf "~a: ~a\n" path (if (check-ok? v) "ok" "failed"))
     (exit (if (check-ok? v) 0 1))]
    [_ (eprintf "usage:\n  racket main.rkt <file.fizz>\n  racket main.rkt verify <file.fizz>\n  racket main.rkt ask <file.fizz> <capabilities-of C | reachable? Cap>\n")
       (exit 2)]))

(module+ main
  (main))

#lang racket

;; effervesce/check/host-python.rkt
;;
;; Turn Python source into racklr UIR so real host code (Django, FastAPI,
;; anything Python) can be fed to the bridge checker. Mirrors load.rkt's
;; approach for .fizz: generate the Python3 parser once per process into the
;; system temp dir (with INDENT/DEDENT layout tokens), then parse + lower on
;; demand.
;;
;;   (python-source->uir src)                  -> uir-block | #f
;;   (python->host-code '(["Api" . "..." ]))   -> (hash component uir)
;;
;; `python-source->uir` returns #f when the source cannot be tokenized or
;; parsed, so callers (and the scanner harness) can distinguish a parse gap
;; from a lowering gap instead of crashing on real-world source.

(require racklr/g4-parse
         racklr/gend-parser
         racklr/lower-python
         racklr/uir
         racket/runtime-path
         racket/file)

(provide python-source->uir
         python->host-code
         py-tokenize py-parse-file_input py-tok-type py-tok-value)

;; The Bart Kiers combined Python3 grammar (lexer + parser in one file), the
;; same one lower-python-test.rkt drives. Resolved relative to this module:
;; effervesce/check/  ->  ../../grammars-v4/python/python3/.
(define-runtime-path python3-grammar
  "../../grammars-v4/python/python3/Python3Parser.g4")

;; Generate the parser once, at module load, like load.rkt does for .fizz.
(define py-parser-path (make-temporary-file "py-parser-~a.rkt"))
(display-to-file
 (generate-parser-module (parse-g4-file python3-grammar)
                         #:source-path (path->string python3-grammar)
                         #:indent-tokens? #t)
 py-parser-path #:exists 'replace)

(define py-tokenize        (dynamic-require py-parser-path 'tokenize))
(define py-parse-file_input (dynamic-require py-parser-path 'parse-file_input))
(define py-tok-type        (dynamic-require py-parser-path 'token-type))
(define py-tok-value       (dynamic-require py-parser-path 'token-value))

;; Parse (via parse-file_input) + lower. Returns #f on a tokenize or parse
;; failure (the lowering gap is a separate concern the scanner checks via
;; `?`-fallback symbols).
(define (python-source->uir src)
  (define tks (py-tokenize src))
  (define res (py-parse-file_input tks 0))
  (and (list? res)
       (= (length res) 2)
       (lower-python (second res) py-tok-type py-tok-value)))

;; (listof (cons component-name python-source)) -> (hash component-name uir).
(define (python->host-code sources)
  (for/hash ([p (in-list sources)])
    (values (car p) (python-source->uir (cdr p)))))

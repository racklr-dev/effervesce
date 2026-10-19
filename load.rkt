#lang racket

;; Turn `.fizz` text into Effervesce UIR. Generates the parser from the
;; grammar once per process, into the system temp dir (not CWD), and lowers
;; the resulting CST.

(require racklr/g4-parse
         racklr/gend-parser
         racklr/lower-effervesce
         racket/runtime-path
         racket/file)

(provide load-effervesce)

(define-runtime-path grammar-path "grammar/Effervesce.g4")

(define parser-path (make-temporary-file "eff-parser-~a.rkt"))
(display-to-file
 (generate-parser-module (parse-g4-file grammar-path)
                         #:source-path (path->string grammar-path))
 parser-path #:exists 'replace)

(define eff-parse       (dynamic-require parser-path 'parse))
(define eff-token-type  (dynamic-require parser-path 'token-type))
(define eff-token-value (dynamic-require parser-path 'token-value))

(define (load-effervesce text)
  (lower-effervesce (eff-parse text) eff-token-value eff-token-type))

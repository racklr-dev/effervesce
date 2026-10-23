#lang racket

;; Failure-position probe for the Python3 front end.
;;
;; The generated recursive-descent parser returns bare #f with no position on a
;; parse failure, which makes triaging the remaining Django PARSE-FAIL files
;; guesswork. This probe regenerates the parser with a `furthest-ok` tracker
;; injected into `ctok` (the single terminal-token accessor every parse rule
;; funnels through), then reports the deepest token index the parser ever looked
;; at before failing, with surrounding token context (index + type + value).
;;
;; Usage:
;;   racket tools/parse-fail-probe.rkt <file.py>...
;;   racket tools/parse-fail-probe.rkt --dir <dir>   (report failures only)

(require racklr/g4-parse
         racklr/gend-parser
         racket/runtime-path
         racket/file
         racket/string)

(define-runtime-path python3-grammar
  "../../grammars-v4/python/python3/Python3Parser.g4")

;; Generate the module source, then inject a furthest-lookahead tracker into
;; `ctok`. `ctok` is the single choke point every generated parse rule uses to
;; read a token, so the max index it is asked to look at is the deepest position
;; the parser reached before bailing. The instrumentation never changes what the
;; parser returns; it only records positions.
(define (instrumented-module-src)
  (define src (generate-parser-module (parse-g4-file python3-grammar)
                                      #:source-path (path->string python3-grammar)
                                      #:indent-tokens? #t))
  (define ctok-old
    (string-append
     "(define (ctok tks pos)\n"
     "  (if (< pos (length tks)) (list-ref tks pos)\n"
     "      (token 'EOF \"\" (source-pos 0 0 0) (source-pos 0 0 0))))"))
  (define ctok-new
    (string-append
     "(define furthest-ok (box -1))\n"
     "(define (ctok tks pos)\n"
     "  (when (> pos (unbox furthest-ok)) (set-box! furthest-ok pos))\n"
     "  (if (< pos (length tks)) (list-ref tks pos)\n"
     "      (token 'EOF \"\" (source-pos 0 0 0) (source-pos 0 0 0))))"))
  (unless (string-contains? src ctok-old)
    (error 'parse-fail-probe
           "could not locate ctok in generated module; instrumentation not applied"))
  (string-append
   (string-replace src ctok-old ctok-new)
   "\n(define (reset-furthest!) (set-box! furthest-ok -1))\n"
   "(define (furthest-position) (unbox furthest-ok))\n"
   "(provide reset-furthest! furthest-position)\n"))

(define probe-parser-path (make-temporary-file "py-failprobe-~a.rkt"))
(display-to-file (instrumented-module-src) probe-parser-path #:exists 'replace)

(define probe-tokenize   (dynamic-require probe-parser-path 'tokenize))
(define probe-parse-file (dynamic-require probe-parser-path 'parse-file_input))
(define probe-tok-type   (dynamic-require probe-parser-path 'token-type))
(define probe-tok-value  (dynamic-require probe-parser-path 'token-value))
(define probe-reset!     (dynamic-require probe-parser-path 'reset-furthest!))
(define probe-furthest   (dynamic-require probe-parser-path 'furthest-position))

;; NOTE: the generated Python tokenizer's position fields are unreliable — the
;; `line` field stays 1, `col` counts characters, and `offset` drifts once
;; INDENT/DEDENT tokens are inserted. Token type/value are trustworthy, so this
;; probe reports token index + type + value only; grep the source for the
;; value sequence to find the exact line.
(define (tok-str i t)
  (format "  [~a] ~a ~s"
          i (probe-tok-type t) (probe-tok-value t)))

(define (probe-file path verbose?)
  (define tks (probe-tokenize (file->string path)))
  (probe-reset!)
  (define res (probe-parse-file tks 0))
  (if (and (list? res) (= (length res) 2))
      (when verbose? (printf "OK    ~a\n" path))
      (let* ([n (length tks)]
             [fur (probe-furthest)]
             [idx (min fur (max 0 (- n 1)))]
             [tok (list-ref tks idx)])
        (printf "FAIL  ~a\n" path)
        (printf "  tokens=~a  deepest-lookahead-index=~a\n" n fur)
        (printf "  failing token:\n~a\n" (tok-str idx tok))
        (printf "  token context:\n")
        (for ([i (in-range (max 0 (- idx 4)) (min n (+ idx 5)))])
          (printf "~a\n" (tok-str i (list-ref tks i)))))))

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

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (match args
    [(list "--dir" dir)
     (for ([f (in-list (sort (find-py-files dir) string<?))])
       (with-handlers ([exn:fail? (λ (e) (printf "ERROR ~a: ~a\n" f (exn-message e)))])
         (probe-file f #f)))]
    [(list)
     (eprintf "usage: parse-fail-probe.rkt (<file.py>... | --dir <dir>)\n")
     (exit 1)]
    [files
     (for ([f (in-list files)])
       (with-handlers ([exn:fail? (λ (e) (printf "ERROR ~a: ~a\n" f (exn-message e)))])
         (probe-file f #t)))]))

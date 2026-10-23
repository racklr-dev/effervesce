#lang racket

;; Scan a directory of Python source through the racklr Python3 pipeline and
;; report, per file and in aggregate, which stage fails:
;;   INDENT-ERROR  — tokenize / indent-insertion raised (layout mismatch)
;;   PARSE-FAIL    — parse-file_input returned #f (grammar gap)
;;   LOWER-ERROR   — lower-python raised on a CST it should handle
;;   FALLBACK      — lowered but with `?`-prefixed unhandled-node symbols
;;   ok            — lowered cleanly; effects=N counts non-empty effect rows
;;
;; This is the honest measurement harness for "can we effervesce real Python
;; (Django) today". Usage:
;;   racket tools/scan-python.rkt <dir> [--verbose]

(require racklr/uir
         racklr/lower-python
         racklr/effects/infer
         racklr/effects/contract
         "../check/host-python.rkt")

;; Does the UIR contain a `?`-prefixed fallback symbol (an unhandled CST node)?
(define (has-fallback? n)
  (cond
    [(uir-symbol? n)
     (define nm (uir-symbol-name n))
     (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\?))]
    [(struct? n)
     (for/or ([f (in-list (cdr (vector->list (struct->vector n))))])
       (has-fallback? f))]
    [(pair? n) (or (has-fallback? (car n)) (has-fallback? (cdr n)))]
    [else #f]))

(define (nonempty-effect-rows program)
  (for/list ([(nm row) (in-hash (infer-effects program))]
             #:when (pair? (uir-effect-row-entries row)))
    (cons nm (map effect-name-symbol (uir-effect-row-entries row)))))

(define (scan-file path py-tokenize py-parse-file py-tok-type py-tok-value verbose)
  (define src (file->string path))
  (define status 'ok)
  (define effects '())
  (define err #f)
  (define fallback? #f)
  (define tks
    (with-handlers ([exn:fail? (λ (e) (set! status 'INDENT-ERROR) (set! err (exn-message e)) #f)])
      (py-tokenize src)))
  (if tks
      (let ([res (py-parse-file tks 0)])
        (if (and (list? res) (= (length res) 2))
            (let ([uir (with-handlers ([exn:fail? (λ (e) (set! status 'LOWER-ERROR) (set! err (exn-message e)) #f)])
                         (lower-python (second res) py-tok-type py-tok-value))])
              (if uir
                  (begin
                    (set! fallback? (has-fallback? uir))
                    (when fallback? (set! status 'FALLBACK))
                    (set! effects (nonempty-effect-rows uir)))
                  (void)))
            (set! status 'PARSE-FAIL)))
      (void))
  (list path status effects err fallback?))

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

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define dir (if (pair? args) (car args) #f))
  (define verbose (member "--verbose" args))
  (unless dir
    (eprintf "usage: scan-python.rkt <dir> [--verbose]\n")
    (exit 1))
  (define files (sort (find-py-files dir) (λ (a b) (string<? (path->string a) (path->string b)))))
  (define rows (map (λ (f) (scan-file f py-tokenize py-parse-file_input py-tok-type py-tok-value verbose)) files))
  (define n (length rows))
  (printf "files=~a\n" n)
  (for ([s (in-list '(ok INDENT-ERROR PARSE-FAIL LOWER-ERROR FALLBACK))])
    (printf "  ~a=~a\n" s (count (λ (r) (equal? (second r) s)) rows)))
  (for ([r (in-list rows)])
    (match-define (list path status effects err fallback?) r)
    (define suffix (if (and err verbose) (format "  (~a)" err) ""))
    (printf "~a  ~a~a\n" status path suffix)
    (when (and verbose (pair? effects))
      (for ([e (in-list effects)])
        (printf "    ~a -> ~a\n" (car e) (map symbol->string (cdr e)))))))

(main)

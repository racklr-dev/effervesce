#lang racket

;; effervesce/tools/verify-dir.rkt
;;
;; One-command bridge for a real spec (R5): absorb + infer every .py file under
;; <dir>, merge into one host program, and run the four checkers (type, policy,
;; model, bridge) against <spec.fizz>. Bridge findings print with the
;; classifying call-site provenance threaded through by R4.
;;
;; Usage:
;;   racket effervesce/tools/verify-dir.rkt <dir> <spec.fizz>
;;         [--component NAME] [--interpret Cap:e1,e2]... [--max-files N]
;;         [--max-size KB]
;;
;; The merged program is keyed under every component declared in the spec (or
;; only --component NAME when given). Interpretations map a capability name to
;; the allowed host effect names; the default is the lowercased capability
;; name, plus the one well-known alias Http -> network. Repeated --interpret
;; flags override the default for that capability.

(require racket/file
         racket/path
         racklr/uir
         "../check/host-python.rkt"
         "../load.rkt"
         "../check/verify.rkt")

(define (find-py-files dir max-files max-size)
  (define out '())
  (let loop ([d dir])
    (for ([e (in-list (directory-list d))])
      (define p (build-path d e))
      (cond
        [(directory-exists? p) (loop p)]
        [(and (regexp-match? #rx"[.]py$" e) (<= (file-size p) max-size))
         (set! out (cons p out))]
        [else (void)])))
  (take (sort out (λ (a b) (string<? (path->string a) (path->string b))))
        (min max-files (length out))))

(define (merge-programs programs)
  (uir-block (apply append (map uir-block-stmts programs))))

(define (spec-component-names sys)
  (for/list ([c (in-list (uir-eff-system-decls sys))]
             #:when (uir-eff-component? c))
    (uir-symbol-name (uir-eff-component-name c))))

(define (default-interpretations sys overrides)
  (define h (make-hash))
  (for ([d (in-list (uir-eff-system-decls sys))]
        #:when (uir-eff-capability? d))
    (define nm (uir-symbol-name (uir-eff-capability-name d)))
    (hash-set! h nm (list (string->symbol (string-downcase nm)))))
  ;; The host effect is named `network`, not `http`.
  (hash-set! h "Http" '(network))
  (for ([(cap effs) (in-hash overrides)])
    (hash-set! h cap effs))
  h)

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define opts (make-hash))
  (define overrides (make-hash))
  (define pos '())
  (let loop ([as args])
    (cond
      [(null? as) (void)]
      [(string=? (car as) "--component")
       (hash-set! opts 'component (second as)) (loop (cddr as))]
      [(string=? (car as) "--interpret")
       (match (regexp-split #rx":" (second as))
         [(list cap effs)
          (hash-set! overrides cap
                     (map string->symbol (regexp-split #rx"," effs)))]
         [_ (eprintf "bad --interpret ~a\n" (second as)) (exit 1)])
       (loop (cddr as))]
      [(string=? (car as) "--max-files")
       (hash-set! opts 'max-files (string->number (second as))) (loop (cddr as))]
      [(string=? (car as) "--max-size")
       (hash-set! opts 'max-size (string->number (second as))) (loop (cddr as))]
      [(string-prefix? (car as) "--") (loop (cdr as))]
      [else (set! pos (cons (car as) pos)) (loop (cdr as))]))
  (when (< (length pos) 2)
    (eprintf "usage: verify-dir.rkt <dir> <spec.fizz> [--component NAME] [--interpret Cap:e1,e2]... [--max-files N] [--max-size KB]\n")
    (exit 1))
  (set! pos (reverse pos))
  (define dir (first pos))
  (define spec-path (second pos))

  (define files
    (find-py-files (simplify-path (path->complete-path dir) #f)
                   (hash-ref opts 'max-files 500)
                   (* 1024 (hash-ref opts 'max-size 300))))
  (define host-code (make-hash))
  (define failed '())
  (for ([f (in-list files)])
    (define src (file->string f))
    (define uir (with-handlers ([exn:fail? (λ (e) #f)])
                  (python-source->uir src)))
    (if uir
        (hash-set! host-code (path->string f) uir)
        (set! failed (cons f failed))))

  (printf "verify-dir: ~a — ~a files (parsed ~a, failed ~a)\n"
          dir (length files) (hash-count host-code) (length failed))
  (when (null? host-code)
    (eprintf "no parsed files\n") (exit 1))

  (define sys
    (with-handlers ([exn:fail?
                     (λ (e) (eprintf "spec load failed: ~a\n" (exn-message e)) (exit 1))])
      (load-effervesce (file->string spec-path))))
  (define interpretations (default-interpretations sys overrides))
  (define merged (merge-programs (hash-values host-code)))
  (define comps (if (hash-has-key? opts 'component)
                    (list (hash-ref opts 'component))
                    (spec-component-names sys)))
  (define host (make-hash))
  (for ([c (in-list comps)]) (hash-set! host c merged))

  (define findings
    (with-handlers ([exn:fail?
                     (λ (e) (eprintf "verify error: ~a\n" (exn-message e)) '())])
      (verify sys host interpretations)))
  (printf "findings: ~a (~a hard)\n"
          (length findings)
          (length (filter (λ (f) (eq? (finding-severity f) 'hard)) findings)))
  (for ([f (in-list findings)])
    (printf "  [~a:~a ~a] ~a\n"
            (finding-checker f) (finding-kind f) (finding-severity f)
            (finding-message f)))
  (exit (if (verify-ok? findings) 0 1)))

(main)

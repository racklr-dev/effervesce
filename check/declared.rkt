#lang racket

;; effervesce/check/declared.rkt
;;
;; Declared-row bridge over REAL host code: run racklr's name-level declared-row
;; check (inferred I ⊆ declared D, from @effects) on actual Python source via
;; the host front end (parse -> lower -> check-declared-rows). This is the
;; static half of the α(O) ≤ I ≤ D sandwich for annotated app code; the runtime
;; guard emitted by emit-python is the dynamic half.
;;
;;   (check-declared-source src)    -> (listof declared-check-finding) | #f
;;   (check-declared-file   path)   -> (listof declared-check-finding) | #f
;;   (declared-ok?          fs)     -> boolean  (no 'hard findings)
;;   (declared-report       fs)     -> (listof string)
;;
;; #f means the source could not be tokenized/parsed/lowered — a front-end gap,
;; not a verdict. Hard violations and unverified notes both come back as
;; findings; `declared-ok?` fails only on 'hard.

(require racklr/uir
         racklr/effects/declared-check
         racket/file
         "host-python.rkt")

(provide check-declared-source
         check-declared-file
         declared-ok?
         declared-report
         ;; Re-export the finding type so callers of the wiring can read
         ;; severities/kinds/messages without reaching into racklr/effects.
         (all-from-out racklr/effects/declared-check))

(define (check-declared-source src)
  (define p (python-source->uir src))
  (and p (check-declared-rows p)))

(define (check-declared-file path)
  (with-handlers ([exn:fail:filesystem? (λ (e) #f)])
    (check-declared-source (file->string path))))

(define (declared-ok? findings)
  (not (for/or ([f (in-list findings)])
         (eq? (declared-check-finding-severity f) 'hard))))

(define (declared-report findings)
  (if (null? findings)
      (list "declared rows OK: every inferred effect is permitted")
      (for/list ([f (in-list findings)])
        (format "[~a ~a] ~a"
                (declared-check-finding-severity f)
                (declared-check-finding-kind f)
                (declared-check-finding-message f)))))

(module+ main
  (define file
    (command-line
     #:program "declared-check"
     #:args (file) file))
  (define fs (check-declared-file file))
  (cond
    [(not fs)
     (eprintf "declared-check: could not read or parse ~a\n" file)
     (exit 2)]
    [else
     (for ([l (in-list (declared-report fs))])
       (displayln l))
     (exit (if (declared-ok? fs) 0 1))]))

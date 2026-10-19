#lang racket

;; The `check()` entry point: run all three checkers over one description and
;; combine their verdicts.

(require racklr/uir
         "load.rkt"
         "check/type.rkt"
         "check/policy.rkt"
         "check/model.rkt")

(provide check check-file check-ok? check-report
         verdict verdict? verdict-type-issues verdict-policy-issues verdict-model-issues)

(struct verdict (type-issues policy-issues model-issues) #:transparent)

(define (check sys)
  (verdict (check-type sys)
           (check-policy sys)
           (check-model sys)))

(define (check-file path)
  (check (load-effervesce (file->string path))))

(define (check-ok? v)
  (and (null? (verdict-type-issues v))
       (null? (verdict-policy-issues v))
       (model-issues-ok? (verdict-model-issues v))))

(define (check-report v)
  (append (check-type-report (verdict-type-issues v))
          (check-policy-report (verdict-policy-issues v))
          (check-model-report (verdict-model-issues v))))

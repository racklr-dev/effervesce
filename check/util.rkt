#lang racket

;; Shared helpers for the checkers: capability-reference identity and text.

(require racklr/uir)

(provide ref-key ref-text)

;; A capability reference's identity: name plus type arguments.
(define (ref-key r)
  (cons (uir-symbol-name (uir-eff-ref-name r))
        (map uir-symbol-name (uir-eff-ref-type-args r))))

(define (ref-text r)
  (define name (uir-symbol-name (uir-eff-ref-name r)))
  (define args (uir-eff-ref-type-args r))
  (if (null? args)
      name
      (string-append name "<" (string-join (map uir-symbol-name args) ", ") ">")))

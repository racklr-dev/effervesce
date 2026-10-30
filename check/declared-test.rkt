#lang racket

;; Declared-row bridge over REAL source: parse -> lower -> check-declared-rows.
;; Exercises the wiring in declared.rkt with actual annotated Python text, so
;; the verdict a developer gets from the CLI is what is tested here.

(require rackunit
         racklr/uir
         "declared.rkt")

(define clean-src
  (string-append
   "from lib import llm\n"
   "@effects({\"llm\": {\"max\": 5}})\n"
   "def f():\n"
   "    llm.query()\n"))

(define violating-src
  (string-append
   "from lib import llm\n"
   "@effects({\"db\": None})\n"
   "def f():\n"
   "    llm.query()\n"))

(define higher-order-src
  (string-append
   "@effects({\"db\": None})\n"
   "def f(cb):\n"
   "    cb()\n"))

(define dynamic-declared-src
  (string-append
   "@effects({\"dynamic\": None})\n"
   "def f(cb):\n"
   "    cb()\n"))

(define (test-clean-source-passes)
  (define fs (check-declared-source clean-src))
  (check-not-false fs "clean source must parse and lower")
  (check-equal? fs '())
  (check-true (declared-ok? fs))
  (check-true (string-contains? (first (declared-report fs)) "OK")))

(define (test-violating-source-is-hard)
  (define fs (check-declared-source violating-src))
  (check-not-false fs "violating source must parse and lower")
  (check-equal? (length fs) 1)
  (check-equal? (declared-check-finding-severity (first fs)) 'hard)
  (check-equal? (declared-check-finding-kind (first fs)) 'violation)
  (check-true (string-contains? (declared-check-finding-message (first fs)) "llm"))
  (check-false (declared-ok? fs))
  (check-true (string-contains? (first (declared-report fs)) "violation")))

(define (test-higher-order-is-dynamic-violation)
  ;; A callback param now classifies as the top `dynamic` domain, so a declared
  ;; row that does not permit `dynamic` is a hard violation (dynamic code can
  ;; perform any effect), not a soft unverified note.
  (define fs (check-declared-source higher-order-src))
  (check-not-false fs "higher-order source must parse and lower")
  (check-equal? (length fs) 1)
  (check-equal? (declared-check-finding-severity (first fs)) 'hard)
  (check-equal? (declared-check-finding-kind (first fs)) 'violation)
  (check-true (string-contains? (declared-check-finding-message (first fs)) "dynamic"))
  (check-false (declared-ok? fs)))

(define (test-declared-dynamic-passes)
  ;; A declared row that explicitly permits `dynamic` covers the callback
  ;; param: the discharge obligation is satisfiable.
  (define fs (check-declared-source dynamic-declared-src))
  (check-not-false fs "dynamic-declared source must parse and lower")
  (check-equal? fs '())
  (check-true (declared-ok? fs)))

(define (test-parse-failure-is-false)
  (check-false (check-declared-source "def f(:\n")))

(module+ main
  (test-clean-source-passes)
  (test-violating-source-is-hard)
  (test-higher-order-is-dynamic-violation)
  (test-declared-dynamic-passes)
  (test-parse-failure-is-false)
  (displayln "All declared (real-source) tests passed."))

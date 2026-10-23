#lang racket

;; End-to-end: materialize host UIR ASTs and their S-expression form into
;; the hypergraph, query the calls they make, and verify the system against
;; a .fizz spec.

(require rackunit
         racklr/uir
         racklr/hypergraph
         "../load.rkt"
         "graph.rkt"
         "query.rkt"
         "verify.rkt")

(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))

(define spec-text
  "system S {
     capability Db { query() -> Unit }
     given Db
     component Api needs Db { run() = query() }
   }")

;; One declared call (db.execute -> db) and one undeclared call (fetch -> network).
(define host-code
  (hash "Api" (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))
                    (fn "fetcher" (uir-call (v "fetch") '())))))

(define sys (load-effervesce spec-text))
(define idx (make-index (system-graph+host sys host-code (hash "Db" '(db)))))

;; ── materialize: the struct AST ─────────────────────────────────────
(define (test-materialize-ast)
  (define atoms (materialize (hash-ref host-code "Api")))
  (define m (make-index atoms))
  (check-true (> (length (index-nodes m 'uir-call)) 0))
  (check-true (> (length (index-nodes m 'uir-fn)) 0))
  (check-true (> (length (index-links m 'slot)) 0)))

;; ── materialize: the S-expression form ──────────────────────────────
(define (test-materialize-sexp)
  (define atoms (materialize (uir->sexp (hash-ref host-code "Api"))))
  (define m (make-index atoms))
  ;; uir->sexp yields a nested list; "fetch" is a str leaf, not a struct vertex.
  (check-true
   (for/or ([a (in-list atoms)])
     (and (node? a) (eq? (node-type a) 'str) (equal? (node-name a) "fetch"))))
  (check-true (> (length (index-nodes m 'list)) 0)))

;; ── query: what does the host AST call? ─────────────────────────────
(define (test-calls-of)
  (define calls (calls-of idx))
  (check-not-false (member "execute" calls))
  (check-not-false (member "fetch" calls)))

;; ── verify: the undeclared fetch call is a hard bridge violation ────
(define (test-verify-undeclared-effect)
  (define fs (verify sys host-code (hash "Db" '(db))))
  (check-false (verify-ok? fs))
  (check-true
   (for/or ([f (in-list fs)])
     (and (eq? (finding-checker f) 'bridge)
          (eq? (finding-kind f) 'bridge-violation)
          (eq? (finding-severity f) 'hard)))))

(module+ main
  (test-materialize-ast)
  (test-materialize-sexp)
  (test-calls-of)
  (test-verify-undeclared-effect)
  (displayln "All AST materialization tests passed."))
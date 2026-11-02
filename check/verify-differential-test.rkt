#lang racket

;; Differential suite: the query-shaped `verify` (typed traversals over the
;; merged substrate) must report exactly the findings the struct-level
;; checkers report. This is the equivalence proof for the P4.3 re-expression:
;; the two paths share the finding text/severity source (check/verify.rkt's
;; normalizers), and this suite pins their agreement over every shipped
;; example plus synthetic specs that exercise each finding kind.

(require rackunit
         racket/file
         racket/list
         racket/string
         racklr/uir
         "../load.rkt"
         "verify.rkt")

(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))

;; A finding set is compared as a sorted multiset of rendered findings: the
;; two paths may emit the same findings in a different order (the graph walk
;; is index-ordered), but never a different finding.
(define (finding-key f)
  (format "~a|~a|~a|~a"
          (finding-checker f)
          (finding-severity f)
          (finding-kind f)
          (finding-message f)))

(define (finding-multiset fs) (sort (map finding-key fs) string<?))

(define (check-same label sys host-code interps)
  (check-equal? (finding-multiset (verify sys host-code interps))
                (finding-multiset
                 (verify-checker-findings sys host-code interps))
                label))

;; ── fixtures ────────────────────────────────────────────────────────

(define clean-spec
  (load-effervesce
   "system S {
      capability Db { query() -> Unit }
      given Db
      component Api needs Db { run() = query() }
    }"))

(define clean-code
  (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))))

(define leaky-code
  (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))
        (fn "fetcher" (uir-call (v "fetch") '()))))

(define unresolved-code
  (prog (fn "handler" (uir-call (v "callback") '()))))

(define hole-spec
  (load-effervesce
   "system S {
      capability Store { add(text: String) -> Todo }
      component App needs Store { addTodo(text) = add(text) }
    }"))

(define note-spec
  (load-effervesce
   "system M {
      capability C { get() -> Int }
      given C
      state cache: Int = get()
      always cache > 0
    }"))

;; Handler completeness, provider completeness, handler/component invocation,
;; and policy violations in one system.
(define completeness-spec
  (load-effervesce
   "system T {
      capability Store {
        add(t: String) -> Unit
        list() -> Unit
      }
      capability Log { write(m: String) -> Unit }
      capability Db { insert(t: String) -> Unit }
      given Log, Db
      Store via Log { add(t) = write(t) }
      Store via Db { list() = insert(t) }
      component App needs Store { run() = add(t) }
      component Leak needs Store { go() = write(t) }
      component Peer provides Store { add(t) = write(t) }
      never component needs Store
    }"))

;; A handler body that invokes a capability op outside its own `via` row.
(define handler-invocation-spec
  (load-effervesce
   "system U {
      capability Store { add(t: String) -> Unit }
      capability Log { write(m: String) -> Unit }
      capability Db { insert(t: String) -> Unit }
      given Log, Db
      Store via Log { add(t) = insert(t) }
      component App needs Store { run() = add(t) }
    }"))

;; Every structural model finding: duplicate state, undeclared write, and an
;; invariant over an undeclared state.
(define model-structural-spec
  (load-effervesce
   "system S {
      state cache: Int = 0
      state cache: Int = 1
      action Load { count := 1 }
      always missing > 0
    }"))

;; A peer-provided handling path, so the handling query and the checker agree
;; on the transitive case too.
(define peer-spec
  (load-effervesce
   "system P {
      capability Store { add(text: String) -> Todo }
      capability Db { insert(text: String) -> Unit }
      given Db
      component Database needs Db provides Store { add(text) = insert(text) }
      component App needs Store { addTodo(text) = add(text) }
    }"))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-shipped-examples)
  (for ([p (in-list (sort (filter (λ (p) (regexp-match? #rx"[.]fizz$" p))
                                   (map (λ (e) (build-path "examples" e))
                                        (directory-list "examples")))
                          path<?))])
    (check-same (path->string p) (load-effervesce (file->string p)) (hash) (hash))))

(define (test-bridge)
  (check-same "clean bridge" clean-spec (hash "Api" clean-code) (hash "Db" '(db)))
  (check-same "bridge violation" clean-spec (hash "Api" leaky-code) (hash "Db" '(db)))
  (check-same "bridge unresolved" clean-spec (hash "Api" unresolved-code) (hash "Db" '(db)))
  (check-same "bridge no host" clean-spec (hash) (hash)))

(define (test-type)
  (check-same "unhandled need" hole-spec (hash) (hash))
  (check-same "completeness + invocation" completeness-spec (hash) (hash))
  (check-same "handler invocation" handler-invocation-spec (hash) (hash))
  (check-same "peer handling" peer-spec (hash) (hash)))

(define (test-policy)
  (define p
    (load-effervesce
     "system P {
        capability Store { add() -> Unit }
        given Store
        component App needs Store { run() = add() }
        must component needs Log
        never component needs Store
      }"))
  (check-same "policy must+never" p (hash) (hash)))

(define (test-model)
  (check-same "model note" note-spec (hash) (hash))
  (check-same "model structural" model-structural-spec (hash) (hash)))

(module+ main
  (test-shipped-examples)
  (test-bridge)
  (test-type)
  (test-policy)
  (test-model)
  (displayln "All verify differential tests passed."))

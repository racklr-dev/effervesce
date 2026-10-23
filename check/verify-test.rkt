#lang racket

;; System 2 verify tests: the four checkers normalized to one finding list,
;; with hard/note severity driving verify-ok?.

(require rackunit
         racklr/uir
         racket/file
         "../load.rkt"
         "verify.rkt")

(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))

(define clean-spec
  (load-effervesce
   "system S {
      capability Db { query() -> Unit }
      given Db
      component Api needs Db { run() = query() }
    }"))

(define clean-code (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))))

(define (test-clean-passes)
  (check-true (verify-ok? (verify clean-spec (hash "Api" clean-code) (hash "Db" '(db))))))

(define (test-bridge-violation-is-hard)
  (define leaky-code
    (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))
          (fn "fetcher" (uir-call (v "fetch") '()))))
  (define fs (verify clean-spec (hash "Api" leaky-code) (hash "Db" '(db))))
  (check-false (verify-ok? fs))
  (check-true (for/or ([f (in-list fs)])
                (and (eq? (finding-checker f) 'bridge)
                     (eq? (finding-kind f) 'bridge-violation)
                     (eq? (finding-severity f) 'hard)))))

(define (test-unhandled-need-is-hard)
  (define bad-spec
    (load-effervesce
     "system S {
        capability Store { add(text: String) -> Todo }
        component App needs Store { addTodo(text) = add(text) }
      }"))
  (define fs (verify bad-spec (hash) (hash)))
  (check-false (verify-ok? fs))
  (check-true (for/or ([f (in-list fs)])
                (and (eq? (finding-checker f) 'type)
                     (eq? (finding-kind f) 'unhandled-need)))))

(define (test-model-note-does-not-fail)
  ;; `always cache > 0` over an uninterpreted state init stays unknown, so it
  ;; is a note (invariant-unverified), not a hard failure.
  (define note-spec
    (load-effervesce
     "system M {
        capability C { get() -> Int }
        given C
        state cache: Int = get()
        always cache > 0
      }"))
  (define fs (verify note-spec (hash) (hash)))
  (check-true (verify-ok? fs))
  (check-true (for/or ([f (in-list fs)])
                (and (eq? (finding-checker f) 'model)
                     (eq? (finding-severity f) 'note)))))

(define (test-config-reader-example-clean)
  ;; The shipped example should verify clean under a spec-only verify.
  (define sys (load-effervesce (file->string "examples/config-reader.fizz")))
  (check-true (verify-ok? (verify sys (hash) (hash)))))

(module+ main
  (test-clean-passes)
  (test-bridge-violation-is-hard)
  (test-unhandled-need-is-hard)
  (test-model-note-does-not-fail)
  (test-config-reader-example-clean)
  (displayln "All verify tests passed."))

#lang racket

(require rackunit
         racket/runtime-path
         racklr/uir
         "type.rkt"
         "../load.rkt")

(define-runtime-path examples-dir "../examples")

(define (example-sys name)
  (load-effervesce (file->string (build-path examples-dir name))))

(define (check-file-ok? name)
  (check-true (type-check-ok? (example-sys name))
              (format "type check ~a" name)))

(check-file-ok? "config-reader.fizz")
(check-file-ok? "todo-store.fizz")
(check-file-ok? "todo-memory.fizz")
(check-file-ok? "todo-database.fizz")
(check-file-ok? "todo-rules.fizz")

;; A bare unhandled need.
(define unhandled
  (load-effervesce
   "system Unhandled {
      capability Store { add() -> Unit }
      component App needs Store { addTodo() = add() }
    }"))
(check-equal? (length (check-type unhandled)) 1)
(check-equal? (unhandled-need-component (first (check-type unhandled))) "App")
(check-equal? (uir-symbol-name
               (uir-eff-ref-name (unhandled-need-capability (first (check-type unhandled)))))
              "Store")

;; `via` only satisfies a need if its own row is satisfiable.
(define broken-via
  (load-effervesce
   "system Broken {
      capability A { x() -> Unit }
      capability B { y() -> Unit }
      B via A { y() = x() }
      component C needs B { run() = y() }
    }"))
(check-equal? (length (check-type broken-via)) 1)

;; A via cycle terminates and counts as unhandled.
(define cycle
  (load-effervesce
   "system Cycle {
      capability A { x() -> Unit }
      capability B { y() -> Unit }
      A via B { x() = y() }
      B via A { y() = x() }
      component C needs A { run() = x() }
    }"))
(check-equal? (length (check-type cycle)) 1)

;; Invocation check: a component body calling an op outside its `needs`.
;; Reachability passes (Store is given); only the invocation is flagged.
(define leak
  (load-effervesce
   "system Leak {
      capability Store { add() -> Unit }
      capability Notify { tell() -> Unit }
      given Store, Notify
      component App needs Store { run() = tell() }
    }"))
(check-equal? (length (check-type leak)) 1)
(check-true (unhandled-invocation? (first (check-type leak))))
(check-equal? (unhandled-invocation-component (first (check-type leak))) "App")
(check-equal? (unhandled-invocation-capability (first (check-type leak))) "Notify")
(check-equal? (unhandled-invocation-op (first (check-type leak))) "tell")

;; The shipped todo-leak.fizz demonstrates the same leak.
(check-true
 (unhandled-invocation? (first (check-type (example-sys "todo-leak.fizz")))))

;; Covered invocations (caller needs the op's capability) add no finding.
(define covered
  (load-effervesce
   "system Covered {
      capability Store { add() -> Unit }
      given Store
      component App needs Store { run() = add() }
    }"))
(check-true (type-check-ok? covered))

;; Handler completeness: a `via` clause must implement every op the capability
;; declares. Here `Store` declares `done` but the handler omits it.
(define incomplete-handler
  (load-effervesce
   "system H {
      capability Store { add() -> Unit done() -> Unit }
      capability Db { insert() -> Unit }
      given Db
      Store via Db { add() = insert() }
      component App needs Store { run() = add() }
    }"))
(check-equal? (length (check-type incomplete-handler)) 1)
(check-true (missing-handler-op? (first (check-type incomplete-handler))))
(check-equal? (missing-handler-op-capability (first (check-type incomplete-handler))) "Store")
(check-equal? (missing-handler-op-op (first (check-type incomplete-handler))) "done")

;; A complete handler adds no finding.
(define complete-handler
  (load-effervesce
   "system H2 {
      capability Store { add() -> Unit done() -> Unit }
      capability Db { insert() -> Unit mark() -> Unit }
      given Db
      Store via Db { add() = insert() done() = mark() }
      component App needs Store { run() = add() }
    }"))
(check-true (type-check-ok? complete-handler))

;; Handler-body leak: `Store via Db` depends only on Db, but its add() body
;; calls Notify.tell. Mirror of the component leak, one level up.
(define handler-leak
  (load-effervesce
   "system HL {
      capability Store { add() -> Unit }
      capability Db { insert() -> Unit }
      capability Notify { tell() -> Unit }
      given Db, Notify
      Store via Db { add() = tell() }
      component App needs Store { run() = add() }
    }"))
(check-equal? (length (check-type handler-leak)) 1)
(check-true (unhandled-handler-invocation? (first (check-type handler-leak))))
(check-equal? (unhandled-handler-invocation-handler (first (check-type handler-leak))) "Store")
(check-equal? (unhandled-handler-invocation-capability (first (check-type handler-leak))) "Notify")
(check-equal? (unhandled-handler-invocation-op (first (check-type handler-leak))) "tell")

;; A handler whose body stays within its own via row adds no finding.
(define covered-handler
  (load-effervesce
   "system HL2 {
      capability Store { add() -> Unit }
      capability Db { insert() -> Unit }
      given Db
      Store via Db { add() = insert() }
      component App needs Store { run() = add() }
    }"))
(check-true (type-check-ok? covered-handler))

;; A concurrent component can satisfy another component's need directly:
;; App needs Store, provided by Database, whose own need Db is given.
(define peer
  (load-effervesce
   "system Peer {
      capability Store { add() -> Unit }
      capability Db { insert() -> Unit }
      given Db
      component Database needs Db provides Store { add() = insert() }
      component App needs Store { run() = add() }
    }"))
(check-true (type-check-ok? peer))

;; A provider whose own needs are unresolved does not satisfy the peer need.
(define peer-hole
  (load-effervesce
   "system PeerHole {
      capability Store { add() -> Unit }
      capability Db { insert() -> Unit }
      component Database needs Db provides Store { add() = insert() }
      component App needs Store { run() = add() }
    }"))
(check-true
 (for/or ([i (in-list (check-type peer-hole))])
   (and (unhandled-need? i)
        (equal? (unhandled-need-component i) "App")
        (equal? (uir-symbol-name (uir-eff-ref-name (unhandled-need-capability i))) "Store"))))

;; A provider must implement every op of each capability it provides.
(define peer-incomplete
  (load-effervesce
   "system PeerIncomplete {
      capability Store { add() -> Unit done() -> Unit }
      capability Db { insert() -> Unit }
      given Db
      component Database needs Db provides Store { add() = insert() }
      component App needs Store { run() = add() }
    }"))
(check-equal? (length (check-type peer-incomplete)) 1)
(check-true (missing-provided-op? (first (check-type peer-incomplete))))
(check-equal? (missing-provided-op-component (first (check-type peer-incomplete))) "Database")
(check-equal? (missing-provided-op-capability (first (check-type peer-incomplete))) "Store")
(check-equal? (missing-provided-op-op (first (check-type peer-incomplete))) "done")

;; A provider cycle terminates and counts as unhandled.
(define peer-cycle
  (load-effervesce
   "system PeerCycle {
      capability A { x() -> Unit }
      capability B { y() -> Unit }
      component P needs B provides A { x() = y() }
      component Q needs A provides B { y() = x() }
      component App needs A { run() = x() }
    }"))
(check-true
 (for/or ([i (in-list (check-type peer-cycle))])
   (and (unhandled-need? i)
        (equal? (unhandled-need-component i) "App")
        (equal? (uir-symbol-name (uir-eff-ref-name (unhandled-need-capability i))) "A"))))

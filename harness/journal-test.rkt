#lang racket

;; Tests for the durable modification journal: every modification is recorded
;; with its inferred row and decision, and replaying a recorded journal
;; rebuilds the same system, including from a fresh OS process.

(require rackunit
         racket/file
         racket/port
         racket/system
         racklr/uir
         racklr/effects/contract
         "journal.rkt")

;; ── fixtures ────────────────────────────────────────────────────────

(define (S name) (uir-symbol name))
(define (definition name body) (uir-fn (S name) '() (uir-block body) #f #f))
(define (call callee . args) (uir-call (S callee) args))
(define (return value) (uir-return value))
(define (row-names row) (map effect-name-symbol (uir-effect-row-entries row)))

(define io-reader
  (definition "read_config" (list (return (call "open" (uir-string "settings.ini"))))))

(define net-client
  (definition "fetch_remote" (list (return (call "fetch" (uir-string "https://example.invalid"))))))

;; Calls definitions that are live in the journal, so its row is only correct
;; when inferred against the accepted definition set.
(define caller
  (definition "load_all" (list (return (call "read_config"))
                               (return (call "fetch_remote")))))

;; Same name as io-reader, but pure: an accepted redefinition replaces it.
(define pure-reader
  (definition "read_config" (list (return (uir-number 0)))))

(define danger
  (definition "delete_everything" (list (return (call "os.remove" (uir-string "/tmp/x"))))))

(define mystery
  (definition "call_mystery" (list (return (call "mystery_helper")))))

(define reader-class
  (uir-class (S "Reader") '() '()
             (list (uir-method (S "read") '()
                               (uir-block (list (return (call "open" (uir-string "x")))))
                               'public))))

(define (fresh-journal-path)
  (define path (make-temporary-file "effervesce-journal-~a"))
  (delete-file path)
  path)

;; ── a new journal starts empty ──────────────────────────────────────

(define path (fresh-journal-path))
(define J (open-journal path))
(check-true (journal? J))
(check-equal? (journal-path J) path)
(check-equal? (journal-entries J) '())
(check-equal? (hash-count (journal-system J)) 0)
(check-equal? (file->string path) "(effervesce-journal 1)\n")

;; ── append records the definition, its inferred row, and the decision ──

(define reader-entry (journal-append! J 'io-reader io-reader 'accept))
(define net-entry (journal-append! J 'net-client net-client 'accept))
(define caller-entry (journal-append! J 'caller caller 'accept))

(check-equal? (row-names (journal-entry-row reader-entry)) '(io))
(check-equal? (row-names (journal-entry-row net-entry)) '(network))
;; The row is a whole-system fact: both callees resolve through the live
;; definition set instead of falling back to 'unknown.
(check-equal? (row-names (journal-entry-row caller-entry)) '(io network))

(check-equal? (map journal-entry-name (journal-entries J))
              '(io-reader net-client caller))
(check-equal? (map journal-entry-sequence (journal-entries J)) '(1 2 3))
(check-equal? (hash-count (journal-system J)) 3)
(check-equal? (journal-entry-definition (hash-ref (journal-system J) 'caller)) caller)

;; A method-only definition gets the join of its methods' rows.
(define class-entry (journal-append! J 'reader-class reader-class 'isolate))
(check-equal? (row-names (journal-entry-row class-entry)) '(io))

;; ── refused modifications are audited but not applied ──────────────

(define isolated-entry (journal-append! J 'danger danger 'isolate))
(define rolled-back-entry (journal-append! J 'mystery mystery 'rollback))

(check-equal? (map journal-entry-name (journal-isolated J)) '(reader-class danger))
(check-equal? (map journal-entry-name (journal-rolled-back J)) '(mystery))
(check-false (hash-has-key? (journal-system J) 'danger))
(check-false (hash-has-key? (journal-system J) 'mystery))
(check-equal? (map journal-entry-sequence (journal-entries J)) '(1 2 3 4 5 6))

;; A refused proposal still records the row inference produced for it.
(check-equal? (row-names (journal-entry-row rolled-back-entry)) '(unknown))

;; ── an accepted redefinition replaces the live definition ──────────

(define redefined-entry (journal-append! J 'io-reader pure-reader 'accept))
(check-equal? (journal-entry-sequence redefined-entry) 7)
(check-equal? (row-names (journal-entry-row redefined-entry)) '())
(check-equal? (journal-entry-definition (hash-ref (journal-system J) 'io-reader)) pure-reader)
;; Append-only: the replaced definition is still in the log, not overwritten.
(check-equal? (length (journal-entries J)) 7)
(check-equal? (journal-entry-decision (last (journal-entries J))) 'accept)

;; ── replay rebuilds the recorded system ────────────────────────────

(define recorded-entries (journal-entries J))
(define recorded-system (journal-system J))
(define recorded-isolated (journal-isolated J))
(define recorded-rolled-back (journal-rolled-back J))
(define recorded-sexp (journal-system->sexp J))
(close-journal J)

(define replayed (read-journal path))
(check-equal? (journal-entries replayed) recorded-entries)
(check-equal? (journal-system replayed) recorded-system)
(check-equal? (journal-isolated replayed) recorded-isolated)
(check-equal? (journal-rolled-back replayed) recorded-rolled-back)
(check-equal? (journal-system->sexp replayed) recorded-sexp)
;; Header plus seven modifications.
(check-equal? (length (file->lines path)) 8)

;; Replay is the state of record: the row inferred for the caller at
;; proposal time survives even though io-reader has since been redefined.
(check-equal? (row-names
               (journal-entry-row (hash-ref (journal-system replayed) 'caller)))
              '(io network))

;; ── a restarted journal resumes and continues the sequence ─────────

(define resumed (open-journal path))
(check-equal? (journal-system resumed) recorded-system)
(define later-entry (journal-append! resumed 'net-client net-client 'rollback))
(check-equal? (journal-entry-sequence later-entry) 8)
(check-equal? (journal-entry-decision later-entry) 'rollback)
;; The refused re-modification leaves the accepted definition in force.
(check-equal? (journal-entry-definition (hash-ref (journal-system resumed) 'net-client))
              net-client)
(close-journal resumed)

(define replayed-again (read-journal path))
(check-equal? (journal-entries replayed-again)
              (append recorded-entries (list later-entry)))
(check-equal? (map journal-entry-name (journal-rolled-back replayed-again))
              '(mystery net-client))

;; ── replay in a fresh OS process rebuilds the same system ──────────

;; The suite runs from effervesce/, so the run file is
;; effervesce/harness/journal-test.rkt and three levels up is the checkout root
;; that holds the racklr and effervesce collections.
(define (checkout-root)
  (simplify-path
   (build-path (path->complete-path (find-system-path 'run-file)) 'up 'up 'up)))

(define (collection-environment)
  (define env (environment-variables-copy (current-environment-variables)))
  (define root (path->string (checkout-root)))
  (define existing (or (getenv "PLTCOLLECTS") ""))
  (environment-variables-set!
   env #"PLTCOLLECTS"
   (string->bytes/utf-8
    (if (string=? existing "")
        (string-append root ":")
        (string-append root ":" existing))))
  env)

(define (replayed-system-sexp-in-child path)
  (define program
    (format
     "(require effervesce/harness/journal) (write (journal-system->sexp (read-journal ~s)))"
     (path->string path)))
  (define out (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-environment-variables (collection-environment)])
      (system*/exit-code
       (find-executable-path (find-system-path 'exec-file))
       "-e" program)))
  (unless (zero? code)
    (error 'journal-test "replay in a fresh process exited ~a" code))
  (read (open-input-string (get-output-string out))))

(check-equal? (replayed-system-sexp-in-child path)
              (journal-system->sexp (read-journal path)))

;; ── a torn append is dropped; corruption before it is an error ─────

(define torn-path (fresh-journal-path))
(define torn (open-journal torn-path))
(define torn-entry (journal-append! torn 'io-reader io-reader 'accept))
(check-equal? (row-names (journal-entry-row torn-entry)) '(io))
(close-journal torn)
(define before-torn-sexp (journal-system->sexp (read-journal torn-path)))

;; Simulate a crash between writing a line and completing it: the partial
;; final line was never acknowledged, so replay resumes before it.
(define torn-out (open-output-file torn-path #:exists 'append))
(display "(modification 2 io-reader" torn-out)
(close-output-port torn-out)
(define recovered (read-journal torn-path))
(check-equal? (journal-system->sexp recovered) before-torn-sexp)
(check-equal? (length (journal-entries recovered)) 1)

;; Reopening for appending repairs the torn tail before writing again: the
;; partial line is dropped, not left to corrupt the next entry.
(define torn-resumed (open-journal torn-path))
(check-equal? (length (journal-entries torn-resumed)) 1)
(define after-torn (journal-append! torn-resumed 'net-client net-client 'accept))
(check-equal? (journal-entry-sequence after-torn) 2)
(close-journal torn-resumed)
(define torn-replayed (read-journal torn-path))
(check-equal? (map journal-entry-sequence (journal-entries torn-replayed)) '(1 2))
(check-equal? (length (file->lines torn-path)) 3)

;; A complete entry whose terminating newline was never written is kept, and
;; reopening terminates the line before appending to it.
(define newline-path (fresh-journal-path))
(define newline-journal (open-journal newline-path))
(define first-newline-entry (journal-append! newline-journal 'io-reader io-reader 'accept))
(check-equal? (row-names (journal-entry-row first-newline-entry)) '(io))
(close-journal newline-journal)
(define newline-text (file->string newline-path))
(check-equal? (string-ref newline-text (sub1 (string-length newline-text))) #\newline)
(display-to-file (substring newline-text 0 (sub1 (string-length newline-text)))
                 newline-path #:exists 'replace)
(define newline-resumed (open-journal newline-path))
(check-equal? (length (journal-entries newline-resumed)) 1)
(define after-newline (journal-append! newline-resumed 'net-client net-client 'accept))
(check-equal? (journal-entry-sequence after-newline) 2)
(close-journal newline-resumed)
(check-equal? (map journal-entry-sequence
                   (journal-entries (read-journal newline-path)))
              '(1 2))

;; A complete-but-corrupt entry with more log after it is not a torn tail.
(define corrupt-path (fresh-journal-path))
(display-to-file
 (string-append
  "(effervesce-journal 1)\n"
  "(not a modification entry)\n"
  "(modification 2 x (fn (symbol \"f\") () (block ()) #f #f) (effect-row) accept)\n")
 corrupt-path
 #:exists 'replace)
(check-exn exn:fail? (lambda () (read-journal corrupt-path)))

;; Out-of-order sequence numbers are rejected rather than silently reordered.
(define gap-path (fresh-journal-path))
(display-to-file
 (string-append
  "(effervesce-journal 1)\n"
  "(modification 1 a (fn (symbol \"f\") () (block ()) #f #f) (effect-row) accept)\n"
  "(modification 3 b (fn (symbol \"g\") () (block ()) #f #f) (effect-row) accept)\n")
 gap-path
 #:exists 'replace)
(check-exn exn:fail? (lambda () (read-journal gap-path)))

;; ── append input is validated ─────────────────────────────────────

(define validation-path (fresh-journal-path))
(define validation-journal (open-journal validation-path))
(check-exn exn:fail?
           (lambda () (journal-append! validation-journal 'bad io-reader 'maybe)))
(check-exn exn:fail?
           (lambda () (journal-append! validation-journal "bad name" io-reader 'accept)))
(check-exn exn:fail?
           (lambda () (journal-append! validation-journal 'bad '(not a uir node) 'accept)))
(check-equal? (journal-entries validation-journal) '())
(close-journal validation-journal)

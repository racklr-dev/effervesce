#lang racket

;; effervesce/harness/journal.rkt
;;
;; Durable modification journal for the live system (P5.2).
;;
;; The journal is the append-only record of every modification the gate
;; considered: the definition that was proposed, the effect row inferred for
;; it against the live definition set, and the decision taken about it
;; ('accept, 'isolate, or 'rollback). It is also the system's state of record:
;; a new process resumes by replaying the file, applying accepted definitions
;; in order and retaining the refused modifications for audit.
;;
;; On-disk format, one form per line:
;;
;;   (effervesce-journal 1)
;;   (modification <seq> <name> <definition> <row> <decision>)
;;
;; <definition> and <row> are racklr/uir sexps (see racklr/uir/serialize), so
;; the log is plain data the rest of the tower can read. Replay never
;; re-infers a recorded row: what was inferred and decided is fixed by the
;; log, so replaying a file always rebuilds the same system.
;;
;; Durability: each modification is written unbuffered and flushed to the OS
;; before the append is acknowledged. A crash can still leave a partial final
;; line; replay drops that unacknowledged tail, and reopening truncates it
;; before appending again. Corruption before the tail is a hard error, never
;; silently skipped.

(require racklr/uir
         racklr/effects/infer
         racklr/effects/rows
         racket/file
         racket/match
         racket/string)

(provide (struct-out journal-entry)
         journal? journal-path journal-entries journal-system
         journal-isolated journal-rolled-back
         open-journal read-journal close-journal
         journal-append!
         journal-system->sexp
         definition-effect-row definition-names)

;; ── records ─────────────────────────────────────────────────────────

(define journal-version 1)
(define decisions '(accept isolate rollback))

;; One decision, as recorded. `row` is the row inferred when the definition
;; was proposed; `definition` is the proposed UIR definition itself.
(struct journal-entry (sequence name definition row decision) #:transparent)

;; An open journal. `end` is the byte offset just past the last complete
;; entry (a torn tail lies beyond it); `out` is the append port when the
;; journal is open for writing, and #f for a replay-only handle. `entries` is
;; oldest-first; the system, isolated and rolled-back lists are the replay of
;; those entries.
(struct journal (path end entries system isolated rolled-back out)
  #:transparent #:mutable)

;; ── serialization ───────────────────────────────────────────────────

(define (entry->sexp entry)
  `(modification ,(journal-entry-sequence entry)
                 ,(journal-entry-name entry)
                 ,(uir->sexp (journal-entry-definition entry))
                 ,(uir->sexp (journal-entry-row entry))
                 ,(journal-entry-decision entry)))

;; Parse one line as exactly one datum. Returns 'torn for a line that does not
;; hold a complete datum, so the caller can treat a partial final write as the
;; unacknowledged tail of a crash instead of a journal the system must reject.
(define (parse-line text)
  (with-handlers ([exn:fail? (lambda (e) 'torn)])
    (with-input-from-string text
      (lambda ()
        (define form (read))
        (if (and (not (eof-object? form)) (eof-object? (read)))
            form
            'torn)))))

(define (sexp->entry form line-number)
  (match form
    [`(modification ,sequence ,name ,definition ,row ,decision)
     (unless (exact-nonnegative-integer? sequence)
       (error 'read-journal "line ~a: bad sequence number ~e" line-number sequence))
     (unless (symbol? name)
       (error 'read-journal "line ~a: bad definition name ~e" line-number name))
     (unless (memq decision decisions)
       (error 'read-journal "line ~a: bad decision ~e" line-number decision))
     (define parsed-row (sexp->uir row))
     (unless (uir-effect-row? parsed-row)
       (error 'read-journal "line ~a: entry row is not an effect row" line-number))
     (journal-entry sequence name (sexp->uir definition) parsed-row decision)]
    [_ (error 'read-journal "line ~a: not a modification entry" line-number)]))

(define (write-form! out form)
  (display (format "~s\n" form) out)
  (flush-output out))

;; ── replay ──────────────────────────────────────────────────────────

(define (fold-decisions entries)
  (define system (make-hash))
  (define isolated '())
  (define rolled-back '())
  (for ([entry (in-list entries)])
    (case (journal-entry-decision entry)
      [(accept) (hash-set! system (journal-entry-name entry) entry)]
      [(isolate) (set! isolated (cons entry isolated))]
      [(rollback) (set! rolled-back (cons entry rolled-back))]))
  (values system (reverse isolated) (reverse rolled-back)))

;; Byte offset just past the newline ending physical line `line-number`, or the
;; file size when that line has no terminating newline.
(define (prefix-end-offset path line-number)
  (define bytes (file->bytes path))
  (let loop ([index 0] [line 0])
    (cond
      [(>= line line-number) index]
      [(>= index (bytes-length bytes)) (bytes-length bytes)]
      [else (loop (add1 index)
                  (if (= (bytes-ref bytes index) 10) (add1 line) line))])))

;; Replay a journal file without opening it for appending. Never re-infers a
;; recorded row: the log is the state of record.
(define (read-journal path)
  (unless (file-exists? path)
    (error 'read-journal "no journal at ~a" path))
  (define lines (file->lines path))
  (define forms
    (for/list ([line (in-list lines)]
               [number (in-naturals 1)]
               #:unless (string=? "" (string-trim line)))
      (cons number (string-trim line))))
  (cond
    [(null? forms)
     (if (zero? (file-size path))
         ;; A zero-length file is a journal whose creation was interrupted
         ;; before the header was written; no modification was acknowledged.
         (journal path 0 '() (make-hash) '() '() #f)
         (error 'read-journal "~a: journal has no header line" path))]
    [else
     (define header (parse-line (cdar forms)))
     (unless (equal? header (list 'effervesce-journal journal-version))
       (error 'read-journal
              "~a: not an effervesce journal (expected version ~a)"
              path journal-version))
     (define-values (entries last-line)
       (let loop ([rest (cdr forms)] [sequence 1] [acc '()] [last-line (caar forms)])
         (cond
           [(null? rest) (values (reverse acc) last-line)]
           [else
            (define number (caar rest))
            (define form (parse-line (cdar rest)))
            (cond
              [(eq? form 'torn)
               ;; A partial final line is an append torn by a crash; it was
               ;; never acknowledged, so replay resumes before it.
               (if (null? (cdr rest))
                   (values (reverse acc) last-line)
                   (error 'read-journal "line ~a: corrupt journal entry" number))]
              [else
               (define entry (sexp->entry form number))
               (unless (= (journal-entry-sequence entry) sequence)
                 (error 'read-journal
                        "line ~a: expected sequence ~a, got ~a"
                        number sequence (journal-entry-sequence entry)))
               (loop (cdr rest) (add1 sequence) (cons entry acc) number)])])))
     (define-values (system isolated rolled-back) (fold-decisions entries))
     (journal path (prefix-end-offset path last-line)
              entries system isolated rolled-back #f)]))

;; ── opening ─────────────────────────────────────────────────────────

(define (file-ends-with-newline? path)
  (define bytes (file->bytes path))
  (and (positive? (bytes-length bytes))
       (= (bytes-ref bytes (sub1 (bytes-length bytes))) 10)))

(define (open-append-port path)
  (define parent (path-only path))
  (when parent (make-directory* parent))
  (define out (open-output-file path #:exists 'append #:mode 'text))
  (file-stream-buffer-mode out 'none)
  ;; A crash can leave a complete entry whose terminating newline was not
  ;; written; terminate that line before appending so the next entry does not
  ;; run into it.
  (unless (or (zero? (file-size path)) (file-ends-with-newline? path))
    (display "\n" out)
    (flush-output out))
  out)

;; Drop a torn tail from a journal file before reopening it for appending. The
;; complete prefix is copied to a sibling temporary file and renamed over the
;; journal, so the repaired file is either the old one or the truncated one.
(define (truncate-journal! path end)
  (define bytes (file->bytes path))
  (when (< end (bytes-length bytes))
    (define directory (or (path-only path) (current-directory)))
    (define temporary
      (make-temporary-file "effervesce-journal-~a" #f directory))
    (display-to-file (subbytes bytes 0 end) temporary #:exists 'truncate)
    (rename-file-or-directory temporary path #t)))

(define (open-fresh! path)
  (define out (open-append-port path))
  (write-form! out (list 'effervesce-journal journal-version))
  (journal path (file-size path) '() (make-hash) '() '() out))

(define (open-existing! path)
  (define journal (read-journal path))
  (truncate-journal! path (journal-end journal))
  (set-journal-out! journal (open-append-port path))
  journal)

;; Open a journal for appending, resuming from the entries already on disk (or
;; starting a new log when the file is absent or empty). This is the restart
;; path: the returned journal carries the system the recorded run had.
(define (open-journal path)
  (cond
    [(not (file-exists? path)) (open-fresh! path)]
    [(zero? (file-size path)) (open-fresh! path)]
    [else (open-existing! path)]))

(define (close-journal journal)
  (define out (journal-out journal))
  (when out
    (close-output-port out)
    (set-journal-out! journal #f)))

;; ── inference ───────────────────────────────────────────────────────

;; Function names a definition introduces. Definitions are UIR programs; the
;; row recorded for one is the join of the rows of the functions and methods
;; it defines.
(define (definition-names definition)
  (sort (remove-duplicates
         (append (hash-keys (collect-functions definition))
                 (hash-keys (collect-methods definition))))
        symbol<?))

;; Infer `definition` against `context`, a list of definitions that stay live
;; under it. The context is what makes the row a whole-system fact: a proposal
;; that calls a definition accepted earlier inherits that definition's effects
;; instead of falling back to 'unknown.
(define (definition-effect-row definition [context '()])
  (unless (uir? definition)
    (error 'definition-effect-row "not a UIR definition: ~e" definition))
  (define rows (infer-effects (cons definition (filter uir? context))))
  (define names (definition-names definition))
  (cond
    [(null? names) (empty-row)]
    [else
     (for/fold ([row (empty-row)]) ([name (in-list names)])
       (infer-row-union row (hash-ref rows name (empty-row))))]))

;; Accepted definitions still in force when `definition` is proposed. A
;; definition that redefines a function shadows every accepted definition that
;; defines that name, so the shadowed body does not take part in inference.
(define (inference-context journal definition)
  (define shadowed (definition-names definition))
  (define live
    (sort (hash-values (journal-system journal))
          (lambda (a b)
            (string<? (symbol->string (journal-entry-name a))
                      (symbol->string (journal-entry-name b))))))
  (for/list ([entry (in-list live)]
             #:unless
             (for/or ([name (in-list (definition-names (journal-entry-definition entry)))])
               (memq name shadowed)))
    (journal-entry-definition entry)))

;; ── appending ───────────────────────────────────────────────────────

;; Record one modification. The row is inferred here, against the live system,
;; so a journal entry can never disagree with the definition it records; the
;; file append is completed before the in-memory system is updated.
(define (journal-append! journal name definition decision)
  (unless (journal? journal)
    (error 'journal-append! "not a journal: ~e" journal))
  (define out (journal-out journal))
  (unless out
    (error 'journal-append! "journal ~a is not open for appending" (journal-path journal)))
  (unless (symbol? name)
    (error 'journal-append! "definition name must be a symbol: ~e" name))
  (unless (memq decision decisions)
    (error 'journal-append! "decision must be one of ~e: ~e" decisions decision))
  (unless (uir? definition)
    (error 'journal-append! "definition must be a UIR node: ~e" definition))
  (define row (definition-effect-row definition (inference-context journal definition)))
  (define sequence (add1 (length (journal-entries journal))))
  (define entry (journal-entry sequence name definition row decision))
  (write-form! out (entry->sexp entry))
  (set-journal-entries! journal (append (journal-entries journal) (list entry)))
  (case decision
    [(accept) (hash-set! (journal-system journal) name entry)]
    [(isolate) (set-journal-isolated! journal (append (journal-isolated journal) (list entry)))]
    [(rollback) (set-journal-rolled-back! journal (append (journal-rolled-back journal) (list entry)))])
  entry)

;; ── state as data ───────────────────────────────────────────────────

;; Canonical serialization of the replayed system, sorted by name, for
;; inspection and for comparing two replays (including across processes).
(define (journal-system->sexp journal)
  (define live
    (sort (hash-values (journal-system journal))
          (lambda (a b)
            (string<? (symbol->string (journal-entry-name a))
                      (symbol->string (journal-entry-name b))))))
  `(effervesce-system ,@(map entry->sexp live)))

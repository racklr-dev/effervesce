#lang racket

;; effervesce/harness/main.rkt
;;
;; Entry point of the live image: a long-lived process that holds the loaded
;; Effervesce system and serves the eval surface over stdin/stdout. The
;; process stays up across definitions and queries; a redefinition is visible
;; to the next query without a restart.
;;
;; Usage:
;;   racket harness/main.rkt [--spec path.fizz] [--name base]
;;
;; It prints one ready line, then one JSON response per request line until
;; a `{"op":"shutdown"}` request or end of input.

(require racket/cmdline
         racket/file
         json
         "protocol.rkt"
         "process.rkt")

(define (emit response)
  (write-json response (current-output-port))
  (newline)
  (flush-output))

;; One-shot startup failure: report it on the protocol's channel, then exit
;; non-zero so a caller does not mistake a dead live image for a running one.
(define (startup-failure message)
  (emit (hash 'ok #f 'ready #f 'error message))
  (exit 1))

(define (main)
  (define spec-path #f)
  (define definition-name "base")
  (command-line
   #:program "racket harness/main.rkt"
   #:once-each
   [("-s" "--spec")
    path
    "Load this .fizz file as the initial definition"
    (set! spec-path path)]
   [("-n" "--name")
    name
    "Name for the initial definition (default: base)"
    (set! definition-name name)]
   #:args ()
   (void))
  (define h
    (with-handlers ([exn:fail? (λ (e) (startup-failure (exn-message e)))])
      (if spec-path
          (make-harness definition-name (file->string spec-path))
          (make-harness))))
  (emit (hash 'ok #t
              'ready #t
              'pid (harness-pid h)
              'generation (harness-generation h)
              'definitions (harness-definition-names h)))
  (let loop ([h h])
    (define line (read-line (current-input-port)))
    (if (eof-object? line)
        (exit 0)
        (let-values ([(response h* shutdown?) (handle-line h line)])
          (emit response)
          (if shutdown? (exit 0) (loop h*))))))

(module+ main
  (main))

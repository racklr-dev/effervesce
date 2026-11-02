#lang racket

;; effervesce/harness/serialize.rkt
;;
;; P5.3 — Loop as data: serialization.
;;
;; The manifest is already a UIR value (`manifest->uir`); this module wraps that
;; value in the versioned UIR envelope and in JSON on disk, so a loop manifest
;; can be inspected, edited, and read back by the agent like any other artifact.
;;
;; The sexp form is version-free in memory (the envelope carries the schema
;; version). The file form is versioned JSON: `uir->json` writes
;; (uir VERSION payload) and `json->uir` migrates an older payload on read.

(require racklr/uir
         json
         racket/file
         "manifest.rkt")

(provide
 manifest->sexp sexp->manifest
 manifest->json json->manifest
 write-manifest-file! read-manifest-file)

(define (manifest->sexp m)
  (uir->versioned-sexp (manifest->uir m)))

(define (sexp->manifest s)
  (uir->manifest (versioned-sexp->uir s)))

;; `indent` is optional: #f (the default) gives the compact transport string,
;; a positive integer gives a pretty file a human/agent can read and edit.
(define (manifest->json m [indent #f])
  (define compact (uir->json (manifest->uir m)))
  (if indent
      (string-append (jsexpr->string (string->jsexpr compact) #:indent indent) "\n")
      compact))

(define (json->manifest text)
  (uir->manifest (json->uir text)))

(define (write-manifest-file! path m)
  (define path* (path->complete-path path))
  (define-values (dir _file _dir?) (split-path path*))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir))
  (display-to-file (manifest->json m 2) path* #:exists 'replace))

(define (read-manifest-file path)
  (json->manifest (file->string path)))

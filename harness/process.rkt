#lang racket

;; effervesce/harness/process.rkt
;;
;; The live image: a long-lived process over the loaded Effervesce system.
;;
;; The process holds an ordered set of *named definitions*. Each definition is
;; a complete `.fizz` unit (`system Name { ... }`). The live system is the
;; composition of every definition's declarations, so adding or replacing one
;; definition redefines the live system while the process keeps running.
;;
;; State transitions are functional: `harness-eval` returns a new harness and
;; leaves the old one untouched. The caller swaps the result in, which makes a
;; rejected definition (syntax error) atomic by construction — nothing is
;; mutated until the new composed system exists.
;;
;; This is the P5.1 substrate only: it evaluates and queries. The durable
;; journal (P5.2) and the accept/isolate/rollback gate (P5.4) attach to
;; `harness-eval` rather than replacing it.

(require racket/os
         racklr/uir
         "../load.rkt"
         "../check.rkt"
         (rename-in "../check/verify.rkt" [verify check-verify])
         "../check/graph.rkt"
         "../check/query.rkt")

(provide
 ;; live state
 harness harness? harness-pid harness-generation harness-definitions
 harness-system harness-index harness-definition-names harness-describe
 make-harness
 ;; modification
 definition definition? definition-name definition-text definition-system
 harness-eval harness-remove
 ;; query
 harness-query harness-check harness-verify
 ;; declarations
 decl-kind-counts)

;; ── definitions ─────────────────────────────────────────────────────

;; One named `.fizz` unit: the accepted source text and its lowered system.
(struct definition (name text system) #:transparent)

;; One live image. `definitions` keeps insertion order; the live `system` is
;; their composition. `generation` counts accepted modifications.
(struct harness (pid generation definitions system) #:transparent)

(define (compose-definitions defs)
  (uir-eff-system
   (uir-symbol "Live")
   (append* (for/list ([d (in-list defs)])
              (uir-eff-system-decls (definition-system d))))))

;; Lower one definition. Raises on a malformed unit; the caller keeps the old
;; harness, so a bad definition never reaches the live system.
(define (definition-from-text name text)
  (define sys
    (with-handlers ([exn:fail?
                     (λ (e)
                       (error 'harness-eval
                              "definition ~a is not valid Effervesce: ~a"
                              name
                              (exn-message e)))])
      (load-effervesce text)))
  (unless (uir-eff-system? sys)
    (error 'harness-eval "definition ~a did not lower to an Effervesce system" name))
  (definition name text sys))

;; Start a live image, optionally with one definition loaded. Loading the
;; base definition raises on malformed input, so a process that starts is a
;; process with a valid live system.
(define (make-harness [name "base"] [text #f])
  (define defs (if text (list (definition-from-text name text)) '()))
  (harness (getpid) 0 defs (compose-definitions defs)))

;; ── accessors ───────────────────────────────────────────────────────

;; The index System 1 queries run against, rebuilt from the live system.
(define (harness-index h)
  (make-index (system-graph (harness-system h))))

(define (harness-definition-names h)
  (for/list ([d (in-list (harness-definitions h))]) (definition-name d)))

;; A small, serializable description of the live state, for `stat`.
(define (harness-describe h)
  (hash 'pid (harness-pid h)
        'generation (harness-generation h)
        'definitions (harness-definition-names h)
        'decls (decl-kind-counts (harness-system h))))

;; How many declarations of each kind the live (composed) system carries.
(define (decl-kind-counts sys)
  (define decls (uir-eff-system-decls sys))
  (for/fold ([counts (hash 'capabilities 0
                           'givens 0
                           'handlers 0
                           'components 0
                           'states 0
                           'actions 0
                           'invariants 0
                           'policies 0
                           'other 0)])
            ([d (in-list decls)])
    (hash-update
     counts
     (cond
       [(uir-eff-capability? d) 'capabilities]
       [(uir-eff-given? d) 'givens]
       [(uir-eff-handler? d) 'handlers]
       [(uir-eff-component? d) 'components]
       [(uir-eff-state? d) 'states]
       [(uir-eff-action? d) 'actions]
       [(uir-eff-invariant? d) 'invariants]
       [(uir-eff-policy? d) 'policies]
       [else 'other])
     add1)))

;; ── modification ────────────────────────────────────────────────────

;; Add or replace one definition. Returns the new harness; the old one is
;; unchanged, so a caller can discard a failed attempt.
(define (harness-eval h name text)
  (unless (and (string? name) (not (string=? name "")))
    (error 'harness-eval "definition name must be a non-empty string"))
  (unless (string? text)
    (error 'harness-eval "definition text must be a string"))
  (define def (definition-from-text name text))
  (define old (harness-definitions h))
  (define replaced? (for/or ([d (in-list old)]) (string=? name (definition-name d))))
  (define defs
    (if replaced?
        (for/list ([d (in-list old)])
          (if (string=? name (definition-name d)) def d))
        (append old (list def))))
  (harness (harness-pid h)
           (add1 (harness-generation h))
           defs
           (compose-definitions defs)))

;; Drop one definition. Returns the new harness; raises if the name is unknown.
(define (harness-remove h name)
  (define old (harness-definitions h))
  (unless (for/or ([d (in-list old)]) (string=? name (definition-name d)))
    (error 'harness-remove "no such definition: ~a" name))
  (define defs (for/list ([d (in-list old)]
                          #:unless (string=? name (definition-name d)))
                 d))
  (harness (harness-pid h)
           (add1 (harness-generation h))
           defs
           (compose-definitions defs)))

;; ── query ───────────────────────────────────────────────────────────

;; System 1: a closed-form question against the live system's graph index.
(define (harness-query h q)
  (ask (harness-index h) q))

;; The three-checker verdict over the live system.
(define (harness-check h)
  (check (harness-system h)))

;; The four-checker finding list over the live system. Host code and the
;; bridge interpretation table are empty here; the harness has no host
;; artifact yet, so the bridge reports nothing.
(define (harness-verify h)
  (check-verify (harness-system h) (hash) (hash)))

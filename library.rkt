#lang racket

;; effervesce/library.rkt
;;
;; The stupid-simple library surface: three verbs.
;;
;;   (analyze spec-text)                     -> index
;;   (ask index question)                    -> answer (+ confidence)
;;   (verify spec-text host-code interps)    -> (listof finding)
;;
;; `analyze` and `verify` take `.fizz` text (string), not a path; the CLI is a
;; thin wrapper over these. `host-code` and `interps` are optional and default
;; to empty: the bridge checker is skipped when no host code is given.
;;
;; `ask` and `verify` are re-exported from check/query.rkt and
;; check/verify.rkt so the library is the single entry point for both
;; reasoning modes. The query/traversal vocabulary is re-exported too: a
;; caller adds a question by registering a query (anchor + declared edge
;; kinds + collector), not by extending the dispatcher.

(require racklr/uir
         racklr/hypergraph
         "load.rkt"
         "check/graph.rkt"
         "check/scope.rkt"
         "check/query.rkt"
         (rename-in "check/verify.rkt" [verify check-verify]))

(provide
  ;; verbs
  analyze ask verify
  ;; graph/index + AST materialization/query
  system-graph system-graph+host make-index
  materialize calls-of
  ;; answers
  answer answer? answer-value answer-confidence answer-note certain unknown
  ;; queries (a new question is a new query) + contextual scoping
  query query? query-name query-select query-steps query-collect query-host?
  query-registry register-query! find-query run-query single-step no-steps
  hop hop? hop-type hop-from hop-to forward-hop backward-hop slot-hop
  edge-shapes edge-declared? follow traverse closure scope-nodes scope-atoms
  walk-paths
  ;; findings
  finding finding? finding-checker finding-severity finding-kind finding-message
  verify-ok? verify-report
  ;; helpers
  format-ref-key
  ;; atom constructors (for composing artifacts programmatically)
  component-node capability-node effect-node state-node action-node resource-node
  component-needs-link component-provides-link capability-interprets-link
  capability-effect-link given-link action-writes-link fn-effects-link
  effect-touches-link)

;; spec text -> indexed system graph (System 1 substrate).
(define (analyze spec-text)
  (make-index (system-graph (load-effervesce spec-text))))

;; spec text + host code + interpretation table -> findings (System 2).
;; host-code: (hash component-name program). interps: (hash cap-name effect-names).
(define (verify spec-text [host-code (hash)] [interps (hash)])
  (check-verify (load-effervesce spec-text) host-code interps))

(module+ main
  (displayln "effervesce/library — analyze / ask / verify surface loaded."))

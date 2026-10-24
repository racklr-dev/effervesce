#lang racket

;; effervesce/tools/scoreboard.rkt — the R1 census (Lane G).
;;
;; A read-only, diffable report of the numbers every iteration must move.
;; Emits no atoms; takes the already-built graph and prints the invariant
;; scoreboard so the dump is the source of truth, not the prose.

(require racklr/hypergraph)

(provide report-scoreboard)

;; Link types the goal's schema requires. Anything here with a zero count is
;; a schema gap, not a formatting choice. `fn-effect-row` is the per-function
;; half of the coverage gate (schema addition, see type-graph.rkt).
(define required-link-types
  '(component-needs component-provides capability-effect action-writes
    fn-effects effect-touches fn-effect-row))

;; The canonical effect alphabet (R5): every key the checker may emit, even
;; when no code produces it. Enumerating it here makes "llm: 0" visible
;; instead of silently absent.
(define canonical-effects
  '(llm db network cache io process time email))

(define (link-count atoms ty)
  (count (λ (a) (and (link? a) (eq? (link-type a) ty))) atoms))

(define (effect-fns by-name eff)
  (for/sum ([c (in-hash-values (hash-ref by-name eff (hash)))]) c))

(define (effect-files files-by-name eff)
  (length (hash-ref files-by-name eff '())))

(define (report-scoreboard fn-count effectful-fn-count
                           entry-atoms by-name files-by-name
                           fn-effect-atoms fn-row-atoms atoms)
  (define covered-fns
    (remove-duplicates
     (for/list ([l (in-list fn-row-atoms)])
       (node-name (first (link-out l))))))
  (define total-entries (length entry-atoms))
  (define ops-populated
    (count (λ (e) (pair? (node-name (third (link-out e))))) entry-atoms))
  (define budget-populated
    (count (λ (e) (node-name (fourth (link-out e)))) entry-atoms))
  (define scope-populated
    (count (λ (e) (node-name (fifth (link-out e)))) entry-atoms))
  (define modalities
    (remove-duplicates
     (map (λ (e) (node-name (second (link-out e)))) entry-atoms)))

  (printf "\n=== scoreboard (R1) ===\n")
  (printf "named functions:        ~a\n" fn-count)
  (printf "effectful functions:    ~a\n" effectful-fn-count)
  (printf "pure functions:         ~a\n" (- fn-count effectful-fn-count))
  (printf "functions covered:      ~a / ~a  ~a\n"
          (length covered-fns) fn-count
          (if (= (length covered-fns) fn-count) "(PASS)" "(FAIL)"))
  (printf "fn-effects links (component-level): ~a\n" (length fn-effect-atoms))
  (printf "fn-effect-row links (function-level): ~a\n" (length fn-row-atoms))
  (printf "unknown functions:      ~a\n" (effect-fns by-name 'unknown))
  (printf "unknown files:          ~a\n" (effect-files files-by-name 'unknown))

  (printf "canonical effect alphabet (functions / files):\n")
  (for ([eff (in-list canonical-effects)])
    (printf "  ~a: ~a / ~a\n" eff
            (effect-fns by-name eff) (effect-files files-by-name eff)))
  (printf "llm effect legal:       ~a\n"
          (if (memq 'llm canonical-effects) "#t (alphabet)" "#f"))

  (printf "entries with typed ops: ~a / ~a\n" ops-populated total-entries)
  (printf "modalities observed:    ~a\n" modalities)
  (printf "budget=#f entries:      ~a / ~a\n"
          (- total-entries budget-populated) total-entries)
  (printf "scope=#f entries:       ~a / ~a\n"
          (- total-entries scope-populated) total-entries)
  (printf "link-type census (schema):\n")
  (for ([ty (in-list required-link-types)])
    (printf "  ~a: ~a~a\n" ty (link-count atoms ty)
            (if (positive? (link-count atoms ty)) "" "  (MISSING)"))))

#lang racket

;; effervesce/harness/gate.rkt
;;
;; P5.4 — the effect gate: accept / isolate / rollback for self-modification.
;;
;; The living system's definition set is the set of component definitions it
;; is running: a map from component name (from the Effervesce system
;; description) to the UIR program that defines that component. A proposed
;; change is a candidate program for one component. The gate:
;;
;;   1. absorbs the proposal into UIR. Absorption is an injectable front-end
;;      hook; the default absorbs an in-memory UIR value or its serialized
;;      S-expression form, which is the shape a journal replay or the live
;;      image hands over. A source-language front end plugs into the same
;;      argument without editing the gate.
;;   2. infers the candidate's effect row with racklr/effects/infer.
;;   3. checks it against the component's declared capability contract with
;;      check/bridge.rkt: a capability name is mapped through the
;;      interpretation table to the allowed host effect names, and every
;;      uncovered effect keeps the call sites that produced it.
;;
;; Decisions (ROADMAP.md, Phase 5):
;;
;;   accept   — every inferred effect the change performs is covered by its
;;              component's contract. The candidate is committed to the live
;;              definition set; the system has modified itself through the gate.
;;   isolate  — the change is not certifiable: it performs an uncovered effect,
;;              performs a call whose effect cannot be inferred (`unknown`, a
;;              tracked obligation, never a silent default), or targets a
;;              component with no contract in the description. It is refused
;;              before it reaches the live set and kept in quarantine.
;;   rollback — the change was applied to the live image first and the gate's
;;              check then refused it. The previous definition is restored and
;;              the application is undone.
;;
;; Every refusal names the offending effect and the call sites that made the
;; change illegal, e.g. `component Api performs uncovered effect network via
;; fetcher: fetch`. The gate is the contract (STYLE_GUIDE.md), so it fails
;; closed: an effect it cannot infer is a refusal, not a pass.

(require racklr/uir
         racklr/effects/infer
         racklr/effects/contract
         "../check/bridge.rkt")

(provide
 ;; the gate and its live definition set
 gate? gate-make
 gate-spec gate-interpretations
 gate-definitions gate-definition
 gate-quarantine gate-log
 gate-propose gate-apply gate-eval
 ;; a proposed change
 proposal proposal? proposal-component proposal-source
 ;; a refused change kept in quarantine
 isolated isolated? isolated-proposal isolated-decision
 ;; a decision
 decision decision? decision-kind decision-reason decision-component
 decision-effects decision-violations decision-unverified decision-program
 decision-accepted? decision-isolated? decision-rolled-back?
 decision-report
 ;; absorption
 uir-absorber)

;; ── data ────────────────────────────────────────────────────────────

;; A proposed change: a candidate definition of one component. `source` is
;; handed to the gate's absorber; the default accepts UIR or serialized UIR.
(struct proposal (component source) #:transparent)

;; A verdict on one proposed change.
;;
;; kind    'accept | 'isolate | 'rollback
;; reason  'covered | 'uncovered | 'unverified | 'no-contract
;; effects inferred effect names performed by the absorbed candidate
;; violations bridge-violation structs (effect + classifying call sites)
;; unverified "function: call-site" chains for calls left `unknown`
;; program the absorbed candidate; for rollback, the restored definition (#f
;;         when the change introduced the component and was removed)
(struct decision (kind reason component effects violations unverified program)
  #:transparent)

;; A refused change kept out of the live definition set.
(struct isolated (proposal decision) #:transparent)

;; The gate: the contract (spec + interpretation table), the live definition
;; set, the quarantine, and the decision log. `log` is newest-first and is the
;; journal's input: every modification records its proposed definition, its
;; inferred row and its accept/isolate/rollback decision.
(struct gate (spec interpretations defs quarantined journal absorb)
  #:transparent #:mutable)

;; ── absorption ──────────────────────────────────────────────────────

;; Default absorber: a UIR program or node, or a serialized UIR S-expression.
;; Front ends (Python, TypeScript, ...) supply their own `#:absorb` procedure
;; of the same shape; the gate never carries language knowledge.
(define (uir-absorber _component source)
  (define uir
    (cond
      [(and (uir? source) (uir-block? source)) source]
      [(uir? source) (uir-block (list source))]
      [(list? source) (sexp->uir source)]
      [else (raise-argument-error
             'uir-absorber
             "a UIR program, a UIR node, or a serialized UIR S-expression"
             source)]))
  (if (uir-block? uir) uir (uir-block (list uir))))

;; ── construction and accessors ──────────────────────────────────────

(define (gate-make spec interpretations
                   #:definitions [definitions (hash)]
                   #:absorb [absorb uir-absorber])
  (gate spec interpretations (hash-copy definitions) '() '() absorb))

;; A snapshot of the live definition set (component name → UIR program).
(define (gate-definitions gate)
  (hash-copy (gate-defs gate)))

;; The live program defining `component`, or #f.
(define (gate-definition gate component)
  (hash-ref (gate-defs gate) component #f))

;; Refused changes, newest first.
(define (gate-quarantine gate) (gate-quarantined gate))

;; Decisions, newest first: accept, isolate and rollback entries alike.
(define (gate-log gate) (gate-journal gate))

(define (decision-accepted? d) (eq? (decision-kind d) 'accept))
(define (decision-isolated? d) (eq? (decision-kind d) 'isolate))
(define (decision-rolled-back? d) (eq? (decision-kind d) 'rollback))

;; ── checking ────────────────────────────────────────────────────────

;; Effect names inferred anywhere in a program's functions.
(define (program-effect-names program)
  (remove-duplicates
   (append-map (λ (row)
                 (map effect-name-symbol (uir-effect-row-entries row)))
               (hash-values (infer-effects program)))))

;; "function: call-site" chains for calls inference left `unknown`.
(define (unknown-chains program)
  (define prov (box (make-hash)))
  (infer-effects program #:provenance-table prov)
  (sort (for*/list ([(name sites) (in-hash (unbox prov))]
                    [site (in-list (hash-ref sites 'unknown '()))])
          (format "~a: ~a" name site))
        string<?))

;; The description's `component NAME { ... }` declaration, or #f.
(define (component-declaration spec component)
  (for/first ([c (in-list (filter uir-eff-component? (uir-eff-system-decls spec)))]
              #:when (equal? component (uir-symbol-name (uir-eff-component-name c))))
    c))

;; Absorb a proposal's source and decide it against the live contract without
;; touching the definition set.
(define (gate-propose gate proposal)
  (define component (proposal-component proposal))
  (define program ((gate-absorb gate) component (proposal-source proposal)))
  (inspect gate component program))

;; The decision for one absorbed candidate program. `check-bridge` decides
;; coverage (it carries the provenance sites); inference supplies the effect
;; names and the call chains behind an `unknown` leaf.
(define (inspect gate component program)
  (define spec (gate-spec gate))
  (define effects (program-effect-names program))
  (cond
    [(not (component-declaration spec component))
     (decision 'isolate 'no-contract component effects '() '() program)]
    [else
     (define issues
       (check-bridge spec (hash component program) (gate-interpretations gate)))
     (define violations (filter bridge-violation? issues))
     ;; Only an `unknown` row can carry unresolved calls; skip the second
     ;; inference for a program the gate can already certify.
     (define chains (if (memq 'unknown effects)
                        (unknown-chains program)
                        '()))
     (cond
       [(pair? violations)
        (decision 'isolate 'uncovered component effects violations '() program)]
       [(pair? chains)
        (decision 'isolate 'unverified component effects '() chains program)]
       [else
        (decision 'accept 'covered component effects '() '() program)])]))

;; ── transitions ─────────────────────────────────────────────────────

(define (record! gate d)
  (set-gate-journal! gate (cons d (gate-journal gate))))

(define (quarantine! gate proposal d)
  (set-gate-quarantined! gate (cons (isolated proposal d) (gate-quarantined gate))))

(define (commit! gate d)
  (hash-set! (gate-defs gate)
             (decision-component d)
             (decision-program d)))

;; Put a definition back, or drop it when the change introduced it.
(define (restore! gate component previous)
  (if previous
      (hash-set! (gate-defs gate) component previous)
      (hash-remove! (gate-defs gate) component)))

;; Check a proposal and apply the verdict: an accepted change is committed to
;; the live definition set, a refused change is isolated into quarantine and
;; the live set is untouched.
(define (gate-apply gate proposal)
  (define d (gate-propose gate proposal))
  (if (decision-accepted? d)
      (commit! gate d)
      (quarantine! gate proposal d))
  (record! gate d)
  d)

;; The live-image path: the redefinition is applied first (as eval would), then
;; checked. A refusal is rolled back to the previous definition, so a failed
;; self-modification never survives in the live set.
(define (gate-eval gate proposal)
  (define component (proposal-component proposal))
  (define program ((gate-absorb gate) component (proposal-source proposal)))
  (define previous (gate-definition gate component))
  (hash-set! (gate-defs gate) component program)
  ;; A checker failure must not leave an unvetted definition live: restore the
  ;; previous definition before propagating the error.
  (define verdict
    (with-handlers ([exn:fail?
                     (λ (e)
                       (restore! gate component previous)
                       (raise e))])
      (inspect gate component program)))
  (if (decision-accepted? verdict)
      (begin (record! gate verdict) verdict)
      (begin
        (restore! gate component previous)
        (let ([rolled (struct-copy decision verdict
                                   [kind 'rollback]
                                   [program previous])])
          (record! gate rolled)
          rolled))))

;; ── reporting ───────────────────────────────────────────────────────

(define (effects-text effects)
  (if (null? effects)
      "(none)"
      (string-join (map symbol->string effects) ", ")))

(define (violation-text v)
  (format "uncovered effect ~a via ~a"
          (bridge-violation-effect v)
          (string-join (bridge-violation-sites v) ", ")))

(define (refusal-text d)
  (case (decision-reason d)
    [(no-contract) "no contract in the system description"]
    [(uncovered) "an effect its contract does not cover"]
    [(unverified) "calls whose effects cannot be inferred"]
    [else "refused"]))

;; Human-readable lines for a decision, suitable for the journal or the CLI.
;; The first line is the verdict; a refusal adds the named violation and its
;; provenance.
(define (decision-report d)
  (define head
    (case (decision-kind d)
      [(accept)
       (format "accepted change to component ~a (effects: ~a)"
               (decision-component d) (effects-text (decision-effects d)))]
      [(isolate)
       (format "isolated change to component ~a: ~a (effects: ~a)"
               (decision-component d) (refusal-text d)
               (effects-text (decision-effects d)))]
      [(rollback)
       (format "rolled back change to component ~a: ~a"
               (decision-component d) (refusal-text d))]
      [else (format "unknown decision ~a" (decision-kind d))]))
  (append
   (list head)
   (for/list ([v (in-list (decision-violations d))]) (violation-text v))
   (for/list ([chain (in-list (decision-unverified d))])
     (format "unverified call ~a" chain))))

(module+ main
  (displayln "effervesce/harness/gate — effect gate loaded."))

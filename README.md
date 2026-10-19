# Effervesce

Effervesce is a language-agnostic language for describing a system, then
checking it. One description drives three checkers over the same typed model.

The insight is that the tools you named check three different things, and a
typed description can unify them:

- Quint and the Alloy lineage model-check state, transitions, and invariants.
- Snyk IaC and wiz evaluate policy predicates over a declared inventory.
- effect-ts and Koka type-check effect rows, asking whether every capability
  is handled.

Effervesce makes all three first-class: typed effects (capabilities), handlers
(interpretations and policies), and a small state and action core. Components
declare what effects they use and what operations they expose. Handlers wire
effects together. Then `check()` reports unhandled effects, broken invariants,
and policy violations from the same description.

It is check-only. Execution is a later, optional adaptor.

## The model

- `effect E<A>` names a capability with typed operations.
- `handler H : E => R` interprets E in terms of the row R. An empty row is a
  terminal handler: nothing further is required.
- `assume { E }` declares a capability the environment provides directly.
- `component C { uses R; op ... }` declares a consumer of R and its operations.
- `state`, `action`, and `invariant` describe a transition system.
- `policy P { forbid|require predicate }` states a rule over the description.

See `examples/config-reader.eff` for all of it in one small system.

## The checkers

The first milestone is the type checker. It walks the effect rows and answers
one question: is every used effect handled, directly or transitively, down to a
terminal handler or an assumption? Handler completeness falls out of the same
walk: a handler for E must implement every operation E declares.

The second milestone adds the model checker and the policy evaluator. The
model checker explores reachable states and verifies each invariant. The policy
evaluator interprets `forbid`/`require` predicates over the description.

"Effervesce" means "to bubble and fizz" — the tone is friendly, and the
effe- prefix doubles as a nod to effects.

## Status

Grammar sketch, worked example, and design notes. No checker is implemented
yet.

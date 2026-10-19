# Design notes

## Decisions so far

- Check-only core. Runtime execution is a later nice-to-have, reachable either
  by interpreting the AST or by reusing the type checker's model to evaluate
  expressions.
- Ship the effect-row type checker first, then the model checker and policy
  evaluator. `check()` ultimately reports all three results.
- The surface is deliberately small and prose-like. The keywords read like
  English (`given`, `needs`, `via`, `always`, `never`, `must`) so a first
  reading is enough to understand a system.
- The expression sub-language is minimal. Capabilities, wiring, and rows are
  where the checking value is; expressions start as an opaque AST.

## What type checking means here

Handling is a declarative graph, not a dynamic effect stack. A component
`needs` a row. A `via` clause maps one capability to another row. A `given`
declares a terminal capability. The type checker asks: from every used
capability, does there exist a path through `via` clauses that ends at a
`given`? If not, that capability is unhandled.

Handler completeness is the same graph, one level down: a `via` clause for E
must implement every operation E declares.

## Two expression languages

There are two expression surfaces, and they should not be conflated.

- Value expressions (op bodies, `always` invariants) range over runtime values.
- Policy predicates (inside `never` and `must`) range over the description:
  capabilities and components.

The grammar keeps them separate for that reason.

## Why the friendly keywords

The first version used `effect`, `assume`, `handler`, `uses`, `invariant`, and
`policy forbid|require`. Those are accurate but read like a spec. The friendly
set says the same thing in plain words, which lowers the bar for a first
reading without changing the model. The concepts are unchanged:

- `capability` is a typed effect.
- `given` is the old `assume`.
- `E via R` is the old `handler H : E => R`, with the redundant name dropped.
- `needs` is the old `uses`.
- `always` is the old `invariant`.
- `never` and `must` are the old `policy { forbid|require }`.

## Open questions

- How deep should the value expression language get before the first checker?
- Should an unhandled capability on a component be an error or a warning?
- Is the policy predicate surface rich enough for real IaC rules, or does it
  need quantification over components and capabilities?
- Do we want to emit an executable target later (effect-ts, Koka, Rego), and
  if so, which first?

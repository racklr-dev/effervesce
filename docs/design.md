# Design notes

## Decisions so far

- Check-only core. Runtime execution is a later nice-to-have, reachable either
  by interpreting the AST or by reusing the type checker's model to evaluate
  expressions.
- Ship the effect-row type checker first, then the model checker and policy
  evaluator. `check()` ultimately reports all three results.
- The expression sub-language is deliberately minimal. Effects, handlers, and
  rows are where the checking value is; expressions start as an opaque AST.

## What type checking means here

Handling is a declarative graph, not a dynamic effect stack. A component uses a
row. A handler maps one effect to another row. An `assume` is a terminal
capability. The type checker asks: from every used effect, does there exist a
path through handlers that ends at a terminal handler or an assumption? If not,
that effect is unhandled.

Handler completeness is the same graph, one level down: a handler for E must
implement every operation E declares.

## Two expression languages

There are two expression surfaces, and they should not be conflated.

- Value expressions (op bodies, invariants) range over runtime values.
- Policy predicates (inside `forbid`/`require`) range over the description:
  effects, components, and handlers.

The grammar keeps them separate for that reason.

## Open questions

- How deep should the value expression language get before the first checker?
- Should `assume` be required at the top level, or should an unhandled effect
  on a component be a warning rather than an error?
- Is the policy predicate surface rich enough to express real IaC rules, or
  does it need quantification over components and effects?
- Do we want to emit an executable target later (effect-ts, Koka, Rego), and
  if so, which first?

# Effervesce

Effervesce is a small language for describing a system and checking it. You
write down what your system can do (capabilities), who does it (wiring), and
the rules it must obey (invariants and policies). Then `check()` runs three
checks over that one description.

Effervesce means "to bubble and fizz", which is the mood: small, friendly, and
it fizzes when something is wrong.

## Why

The tools you know check three different things, and a typed description can
unify them:

- Quint and the Alloy family model-check states, transitions, and invariants.
- Snyk IaC and wiz evaluate policy predicates over a declared inventory.
- effect-ts and Koka type-check effect rows, asking whether every capability
  is handled.

Effervesce makes all three first-class. A `capability` is a typed effect.
Components say what they `need`. Wiring says how each capability is `given` or
provided `via` another. Then `check()` reports unhandled capabilities, broken
invariants, and policy violations from the same description.

It is check-only. Running it is a later, optional adaptor.

## The model, in five lines

- `capability Read<A> { get() -> A }` names a typed effect.
- `given Fs, Log` says the outside world provides these directly.
- `Read<Config> via Fs { ... }` interprets one capability in terms of another.
- `component LoadConfig needs Read<Config>, Audit { ... }` declares a consumer.
- `always`, `never`, and `must` state the rules the checkers enforce.

See `examples/config-reader.fizz` for all of it in one small system.

## The checkers

The first milestone is the type checker. It walks the capability graph and
answers one question: is every capability a component needs handled, directly
or transitively, down to something `given`? Handler completeness falls out of
the same walk: a `via` clause for E must implement every operation E declares.

The second milestone adds the model checker and the policy evaluator. The
model checker explores reachable states and verifies each `always` invariant.
The policy evaluator checks every `never` and `must` predicate over the
description.

## Status

Grammar sketch, worked example, and design notes. No checker is implemented
yet.

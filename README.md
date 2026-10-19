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

`check()` runs three checks and returns one verdict:

- **Type check.** Walks the capability graph: is every capability a component
  needs handled, directly or transitively, down to something `given`?
- **Policy check.** Evaluates every `never` and `must` predicate over the
  description (`component needs X`, `not`, `and`, `or`).
- **Model check.** Verifies state safety (unique states, declared assignment
  targets, declared invariant references) and attempts to discharge each
  `always` invariant. Invariants that reference uninterpreted capability
  operations are reported as unverified, not silently passed.

## Run

`check()` is a Racket library; the front end lives in racklr, which must be
installed as a linked package. Link this repo the same way, then run the CLI:

```
raco pkg install --link /path/to/effervesce
racket /path/to/effervesce/main.rkt /path/to/effervesce/examples/todo-incomplete.fizz
```

The CLI prints each finding, a final `ok` / `failed` line, and exits non-zero
on failure:

```
  unhandled capability: component App needs Notify
examples/todo-incomplete.fizz: failed
```

## The examples

- `todo-store.fizz` — the capability vocabulary.
- `todo-memory.fizz` — capabilities handed over directly (`given`).
- `todo-database.fizz` — the same `Store` interpreted `via` `Db`.
- `todo-rules.fizz` — invariants and policies exercising all three checkers.
- `todo-incomplete.fizz` — a deliberate hole: `Notify` is needed but never provided.

## Status

Front end (grammar, lowering, emit) and all three checkers are implemented,
with round-trip and checker tests green. Model checking is MVP-depth:
invariants over uninterpreted capabilities are reported as unverified until
capability interpretations land.

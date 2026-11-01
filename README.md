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
- A component can also `provides X` to satisfy a concurrent peer's `needs X`.
- `always`, `never`, and `must` state the rules the checkers enforce.

See `examples/config-reader.fizz` for all of it in one small system.

## The checkers

`check()` runs three checks and returns one verdict:

- **Type check.** Walks the capability graph: is every capability a component
  needs handled, directly or transitively, down to something `given`? A
  capability may also be satisfied by a peer component that `provides` it, so
  concurrent components compose. Also verifies that each component body only
  invokes operations its `needs` covers, and that a provider implements every
  operation of each capability it provides.
- **Policy check.** Evaluates every `never` and `must` predicate over the
  description (`component needs X`, `not`, `and`, `or`).
- **Model check.** Verifies state safety (unique states, declared assignment
  targets, declared invariant references) and explores reachable states from
  `:=` transitions to discharge each `always` invariant. A failing invariant
  is a violation; one that stays over uninterpreted capability operations is
  reported as unverified, not silently passed.
- **Bridge check.** Infers the effect rows of real host code (Python today)
  with `racklr/effects/infer` and compares them against each component's
  `needs` through a capability-interpretation table (capability name -> allowed
  effect names). A performed effect no declared need covers is a violation; an
  unresolvable call is reported unverified, not silently passed.

## The effect gate (self-modification)

`harness/gate.rkt` is the gate a living system redefines itself through
(../ROADMAP.md, Phase 5). The live definition set maps each component to the UIR
program that defines it; a proposed change is a candidate program for one
component. The gate absorbs it (front ends plug in through `#:absorb`; the
default takes UIR or its serialized form), infers its row with the same
inference the bridge uses, and checks it against that component's contract:

```racket
#lang racket
(require effervesce/harness/gate)

(define gate (gate-make spec interpretations #:definitions live-programs))
(gate-apply gate (proposal "Api" new-api-program))   ; accept | isolate
(gate-eval  gate (proposal "Api" new-api-program))   ; live image: accept | rollback
```

- `accept` — every inferred effect is covered; the change is committed to the
  live definition set.
- `isolate` — an uncovered effect, an unresolvable call, or a component with no
  contract; the change never reaches the live set and is kept in quarantine.
- `rollback` — the change was applied to the live image first and then refused;
  the previous definition is restored.

A refusal is not a diagnostic suggestion: `decision-report` names the offending
effect and the exact call sites behind it
(`uncovered effect network via fetcher: fetch`), and `gate-log` records every
decision for the durable journal. `harness/gate-test.rkt` exercises all three paths.

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

The CLI also has two reasoning modes: `ask` for fast closed-form questions
(System 1) and `verify` for the exact four-checker pass (System 2):

```
racket main.rkt ask examples/todo-peer.fizz capabilities-of App
# Store [certain]

racket main.rkt ask examples/todo-peer.fizz reachable? Store
# yes [certain]

racket main.rkt verify examples/config-reader.fizz
#   [model:invariant-unverified] invariant unverified (...)
# examples/config-reader.fizz: ok
```

## The library

The whole thing is three verbs. `(require effervesce/library)` and:

```racket
#lang racket
(require effervesce/library)

(define spec "system S { ... }")

;; System 1: index the description and ask fast questions.
(define idx (analyze spec))
(ask idx '(capabilities-of "App"))      ; => (answer '((Store)) 'certain #f)
(ask idx '(reachable? ("Store")))       ; => (answer #t 'certain #f)

;; System 2: escalate to the exact checkers.
(define findings (verify spec))
(verify-ok? findings)                   ; => #t
```

- `analyze` turns `.fizz` text into an indexed typed hypergraph.
- `ask` answers closed-form questions with a `certain` / `unknown` tag. It
  never guesses: an answer it cannot compute is `unknown`, which is the signal
  to escalate.
- `verify` runs type, policy, model, and bridge checks and returns one
  structured `finding` list. A `hard` finding fails verification; a `note`
  (an invariant the model checker could not discharge) does not.

### Recovering the capability graph from host rows

A `.fizz` system declares its capability graph; host source code has none. The
host projection in `check/graph.rkt` therefore recovers the capability-level
links from the inferred function rows (`check/host-graph.rkt`), using the same
capability identity the checkers use:

- a coarse entry (empty `ops`) makes the component **need** the domain
  capability (`db`), interpreted by that effect (`capability-effect db -> db`);
- a typed op `db<QuerySet>.read` makes it need the concrete class (`QuerySet`),
  interpreted by the op's domain;
- a component **provides** the capability of each class it defines, and the
  class's method rows say which effect domains that capability entails;
- typed ops and scopes become `effect-touches` (the receiver class or the scope
  is the resource; a coarse entry without either names the performing component
  as the coarsest resource), and durable attribute writes become
  `action-writes`.

Declared systems stay authoritative: recovery only fills components a `.fizz`
spec does not declare. `tools/type-graph.rkt` reports the recovered links in
its link-type census, so a host-only corpus run no longer shows the
capability-level graph as empty.

A binary artifact front end (BAP) is planned so the same three verbs work on
compiled programs, not just `.fizz` source; see
`../docs/plans/dark-factory-library.md`.

## Queries are typed traversals

Both reasoning modes share one query model (`check/query.rkt` +
`check/scope.rkt`). A question anchors at a node and scopes to the subgraph
reachable from it along **declared** edge kinds: the link vocabulary in
`check/graph.rkt` is declared once, and a traversal cannot walk an edge the
vocabulary does not define.

- A `hop` names one declared link-type/slot pair (`forward-hop`,
  `backward-hop`, `slot-hop`). `follow` walks one hop, `traverse` chains hops
  into a scope plan, `closure` iterates a hop set, and `scope-atoms` /
  `walk-paths` return the induced subgraph or the provenance paths.
- A `query` is data: `select` anchors it, `steps` scope it, `collect` answers
  it. `ask` resolves a question name through the query registry, so **a new
  question is a new query** (`register-query!`), not a new branch in a
  dispatcher. `(ask idx '(scope-of component "App" component-needs forward))`
  is the generic anchored-scoping form.
- `verify` is a set of exact queries over the same merged graph: handling,
  invocations, handler/provider completeness, policies, and bridge coverage
  with provenance all come from traversals. The one non-walk question —
  bounded reachable-state exploration for invariants — keeps its exact solver
  as a query predicate.

`check/scope-test.rkt` covers the traversal kernel and the extension contract;
`check/verify-differential-test.rkt` pins the query path to the struct-level
checkers' findings.

## The examples

- `todo-store.fizz` — the capability vocabulary.
- `todo-memory.fizz` — capabilities handed over directly (`given`).
- `todo-database.fizz` — the same `Store` interpreted `via` `Db`.
- `todo-peer.fizz` — the same `Store` provided by a concurrent component.
- `todo-rules.fizz` — invariants and policies exercising all three checkers.
- `todo-incomplete.fizz` — a deliberate hole: `Notify` is needed but never provided.
- `todo-leak.fizz` — `App` declares `needs Store` but its body calls
  `Notify.tell`; caught by the invocation check, not by reachability.
- `todo-started.fizz` — a bounded `0 -> 1` transition whose invariant is
  discharged by reachable-state exploration, not reported unverified.
- `real/fastapi`, `real/django`, `real/agno`, `real/langgraph` — small real
  Python samples with matching `.fizz` specs, checked by the bridge checker
  (`racklr-test/bridge-real-test.rkt` runs all four end to end).

## Status

Front end (grammar, lowering, emit) and all four checkers are implemented,
with round-trip and checker tests green. Model checking explores reachable
states from `:=` transitions with a depth and state bound; invariants over
uninterpreted capabilities, and unbounded explorations that hit the bound, are
reported as unverified. Capability interpretations have landed for the bridge
checker: it infers host-code effect rows (Python today) and checks them against
component `needs` for the FastAPI, Django, agno, and LangGraph examples.

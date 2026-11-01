# harness — the live image

The tower runs as a long-lived process. Racket on Chez already gives dynamic
redefinition and eval; this directory provides Effervesce's version of that
property: a process that holds the loaded system and an eval surface over it,
where a redefinition is observable from the next query without a restart.

## What a definition is

A *definition* is one complete `.fizz` unit (`system Name { ... }`), loaded
under a caller-chosen name. The live system is the composition of every
definition's declarations, so:

- `eval` with a fresh name adds a definition (and its declarations) to the
  live system;
- `eval` with an existing name replaces it in place;
- `remove` drops it.

State transitions are functional: `harness-eval` returns a new harness and
leaves the old one untouched. The caller swaps in the result only after the
new definition lowered successfully, so a rejected definition is atomic by
construction and the running system keeps its previous shape.

## Run it

```
racket harness/main.rkt --spec harness/examples/live-base.fizz
```

The process prints one ready line, then one JSON response per request line,
until `shutdown` or end of input. `--name` sets the name of the initial
definition (default `base`).

```
{"ok":true,"ready":true,"pid":1234,"generation":0,"definitions":["base"]}
{"op":"ask","question":["capabilities-of","App"]}
{"confidence":"certain","generation":0,"ok":true,"pid":1234,"value":["Store"]}
{"op":"eval","name":"worker","text":"system WorkerLive { ... }"}
{"decls":{...},"definitions":["base","worker"],"generation":1,"ok":true,"pid":1234,"redefined":false}
{"op":"shutdown"}
{"event":"shutdown","generation":1,"ok":true,"pid":1234}
```

Requests:

| op | fields | result |
| --- | --- | --- |
| `stat` | — | pid, generation, definitions, declaration counts |
| `eval` | `name`, `text` | new generation, `redefined`, definitions, declaration counts |
| `remove` | `name` | new generation, definitions, declaration counts |
| `ask` | `question` (array) | the System 1 value and its confidence |
| `check` | — | `check-ok` and the type/policy/model report |
| `verify` | — | `verify-ok` and the four-checker finding report |
| `shutdown` | — | terminal response; the process exits |

Questions mirror the CLI vocabulary: `capabilities-of`, `effects-of`,
`side-effects-of`, `touches`, `resources-of`, `reachable?`.

Every response carries `pid` and `generation`. A redefinition increments the
generation without changing the pid: the same process, a new live system.

## Library surface

`process.rkt` is usable directly, which is how the later P5 pieces attach:

```racket
(require "harness/process.rkt")

(define h0 (make-harness "base" base-text))
(define h1 (harness-eval h0 "worker" worker-text))   ; raises on bad input
(harness-query h1 '(capabilities-of "Worker"))
(harness-check h1)                                   ; verdict
(harness-verify h1)                                  ; findings
(harness-definition-names h1)
(harness-generation h1)
```

## Test

Run from `effervesce/` (the package suites run from there):

```
cd effervesce
racket harness-test.rkt
```

The suite has two halves. The in-process half covers the harness state, the
JSON protocol, and the atomicity of a rejected definition. The smoke half
starts the real `main.rkt` process, evaluates a definition, redefines it,
queries the result, and asserts the pid never changed — eval-then-query
without a restart. It also asserts that a bad definition leaves the live
system at the previous generation, and that `shutdown` ends the process.

## Scope

This is P5.1 only: the process and the eval surface. Deliberately absent:

- **No durability.** The definitions live in memory; an append-only journal
  with replay is P5.2.
- **No gate.** `eval` accepts anything that lowers; the accept / isolate /
  rollback decision against capability contracts is P5.4. Today the
  `check`/`verify` requests are how a caller observes what the redefinition
  did.
- **No host artifacts.** `verify` runs the bridge with empty host code and an
  empty interpretation table, so it only sees the `.fizz` description.
- Definitions compose at declaration level, which means two definitions that
  declare the same component or capability are legal to load and are reported
  by the checkers rather than silently merged. Named replacement is the
  intended way to change one.

## Files

- `process.rkt` — live system state, `harness-eval`, `harness-remove`, queries.
- `protocol.rkt` — the JSON-line request/response surface.
- `main.rkt` — the long-lived process entry point.
- `examples/live-base.fizz` — fixture used by the smoke test.

# effervesce/harness — the loop as data (P5.3)

The agentic workflow is a UIR manifest, not Racket control flow. An agent (or a
person) can inspect the manifest, rewrite the loop in it, and install the
rewrite only through the same effect gate that guards every other modification.

```
manifest.rkt    the loop-manifest value, its UIR form, the base agentic loop
shape.rkt       reads the loop shape out of the driver UIR
serialize.rkt   versioned UIR sexp and JSON file round-trip
manifest-gate.rkt        installs a proposed manifest only after the effect gate accepts
run.rkt         executes the driver UIR with stub step handlers (trace)
manifest-test.rkt  end-to-end tests, including rewrite-through-the-gate
run-test.rkt    interpreter unit tests (trace order, data-driven loop, errors)
agent-loop.manifest.json  the base loop as a serialized UIR manifest
```

## The manifest

A `loop-manifest` holds six pieces of data:

| field              | meaning                                                        |
|--------------------|----------------------------------------------------------------|
| `name`             | manifest name                                                   |
| `manifest-version` | manifest schema version (migrations are explicit)               |
| `contract`         | `uir-eff-system`: capabilities and which stage needs which      |
| `program`          | `uir-block` of named `uir-fn`s: one per stage plus the driver   |
| `driver`           | name of the driver function; its body is the loop               |
| `interps`          | capability name -> host effect names (the bridge table)         |

The base loop is `base-loop-manifest`:

- stages `observe` (network), `decide` (llm), `act` (io), `verify` (process),
  `tick` (time);
- the driver body in UIR: `observe`, `tick`, then a bounded `while` over
  `round` that runs `decide`, `act`, `verify`, `tick` and advances `round`.

`manifest->uir` maps the value to a UIR node; `serialize.rkt` wraps it in the
versioned UIR envelope (`manifest->sexp`) and the versioned JSON file form
(`write-manifest-file!` / `read-manifest-file`). The checked-in
`agent-loop.manifest.json` is exactly that serialization; `read-manifest-file`
reads it back to an equal manifest.

## The shape

`loop-shape` (through `manifest-loop-shape`) walks the driver's UIR and returns
the loop as a datum, so a rewrite is observable:

```racket
(loop (seq (step "observe")
           (step "tick")
           (while (< round 3)
                  (seq (step "decide") (step "act") (step "verify")
                       (step "tick") (set! "round" (+ round 1))))))
```

## The gate

`gate-manifest installed proposed` accepts `proposed` only when

1. the manifest is structurally complete: every stage is both a program
   function and a contract component, the driver exists, and the manifest
   survives its own UIR round trip; and
2. the same `check/verify.rkt` gate any other modification passes accepts it —
   type, policy, model, and bridge checkers over the manifest's capability
   contract, with each stage's function body as host code. The bridge checker
   infers the stage's actual effect row and refuses an effect its `needs` do
   not cover, naming the effect and the call site.

An accepted verdict carries the proposed manifest; a refused verdict keeps the
installed manifest and carries the findings (or the structural problems). The
gate never mutates an installed manifest in place.

## Running the loop

`run-manifest` interprets the driver UIR with stub step handlers and returns an
ordered trace of executed stages. It is the loop, driven by the manifest data:
change the driver body (reorder stages, move one inside the loop, change the
bound) and the trace changes without editing any harness code.

## Tests

```bash
cd effervesce
PLTCOLLECTS="/path/to/racklr-worktree:" racket harness/manifest-test.rkt
PLTCOLLECTS="/path/to/racklr-worktree:" racket harness/run-test.rkt
```

`manifest-test.rkt` rewrites the base manifest (equal to the checked-in
`agent-loop.manifest.json` after a read-back) through
`gate-manifest` and observes the new loop shape, serializes the accepted
manifest and observes the same shape after reading it back, and checks that an
uncovered effect (email from `decide`) is refused with its provenance.

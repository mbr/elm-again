# elm-again

Pure retry policies and failure tracking. No clocks or effects.

```elm
import Again
import Again.Policy as Policy exposing (Policy(..))

policy =
    ImmediatelyThen
        (ExponentialBackoff { initialDelay = 1000, multiplier = 2, maxDelay = 30000 })

retryDelay =
    Policy.delay policy

( retry, delay ) =
    Again.failed (Again.init policy)
```

`Policy.delay` takes a zero-based retry index and returns milliseconds as a `Float`.
`Again.failed` advances the sequence; `Again.succeeded` resets it.
`Again.failures` counts failures since initialization or the last success.

Develop with `nix develop`, then `./format.sh` and `./check.sh`. `nix build` runs the same checks in a sandbox. Refresh pinned Elm dependencies with `./update-deps.sh`.

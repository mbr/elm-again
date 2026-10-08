# elm-again

Pure retry policies and failure tracking. No clocks or effects.

```elm
import Again
import Again.Policy as Policy
import Again.Schedule as Schedule exposing (Schedule(..))

schedule =
    ImmediatelyThen
        (ExponentialBackoff { initialDelay = 1000, multiplier = 2, maxDelay = 30000 })

policy =
    Policy.init schedule
        |> Policy.withMaxAttempts 3

( retry, delay ) =
    Again.failed (Again.init policy)
```

`Schedule.delay` takes a zero-based retry index and returns milliseconds as a `Float`.
A `Policy` combines a schedule with an `AttemptLimit`: `Unlimited` or `MaxAttempts Int`.
`Policy.init` allows unlimited retries; `Policy.withMaxAttempts` sets a limit including the initial operation.
`Again.failed` advances the sequence and returns a `Maybe Float` delay, or `Nothing` once the limit is reached.
`Again.succeeded` resets the failure count, keeping the policy.
`Again.failures` counts failures since initialization or the last success.

Develop with `nix develop`, then `./format.sh` and `./check.sh`. `nix build` runs the same checks in a sandbox. Refresh pinned Elm dependencies with `./update-deps.sh`.

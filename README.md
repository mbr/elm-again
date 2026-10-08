# elm-again

Pure retry schedules and attempt limits. Callers handle execution and waiting.

```elm
import Again.Policy as Policy
import Again.Schedule as Schedule exposing (Schedule(..))

policy =
    { schedule = Periodic { delay = 1000 }
    , limit = Policy.MaxAttempts 3
    }

firstRetryDelay =
    Schedule.delay policy.schedule 0
```

`Schedule.delay` takes a zero-based retry index and returns milliseconds as a `Float`.
`Policy.init` allows unlimited retries. `MaxAttempts` includes the initial operation.

Develop with `nix develop`, then `./format.sh` and `./check.sh`. `nix build` runs the same checks in a sandbox. Refresh pinned Elm dependencies with `./update-deps.sh`.

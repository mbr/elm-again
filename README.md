# elm-again

A pure retry controller. You supply time, run attempts, and schedule wakeups.

```elm
policy =
    Again.exponential { initialDelay = 1000, multiplier = 2, maxDelay = 30000 }
        |> Again.withJitter 0.2

model =
    Again.init policy seed
```

Call `Again.update now Again.Start model` to begin. Execute `Run` actions and report `Succeeded` or `Failed` with the supplied attempt ID. For `WakeAt`, schedule `TimerElapsed`; `Again.Task.sleepUntil` can handle the wait. Durations are milliseconds; supply nondecreasing time.

Retries are unlimited unless configured otherwise. Successful values remain available across invalidation and retries; check `Again.status` before treating them as current.

For development, use `nix develop`, then `./format.sh` and `./check.sh`. `nix build` runs the same checks in a sandbox. Refresh pinned Elm dependencies with `./update-deps.sh`.

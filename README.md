# elm-again

Retries for tasks and manually managed values, support exponential backoff.

## `Again.Task`

Retry an existing task up to three times, waiting a second between attempts:

```elm
import Again.Policy as Policy
import Again.Schedule exposing (Schedule(..))
import Again.Task
import Task exposing (Task)

run : Task error value -> Cmd (Result error value)
run task =
    task
        |> Again.Task.retry
            { schedule = Periodic { delay = 1000 }
            , limit = Policy.MaxAttempts 3
            }
        |> Task.attempt identity
```

## `Again.Remote`

Track attempts yourself, using the returned delay to schedule the next command:

```elm
import Again.Policy as Policy
import Again.Remote as Remote
import Again.Schedule exposing (Schedule(..))

attempting =
    Remote.init
        { schedule = Periodic { delay = 1000 }
        , limit = Policy.MaxAttempts 3
        }
        |> Remote.started

failure =
    Remote.failed "Offline" attempting
```

`failure` contains the waiting state and a delay of `Just 1000`. When the retry runs and succeeds:

```elm
retrying =
    failure |> Tuple.first |> Remote.started

ready =
    Remote.ok "Hello" retrying

value =
    Remote.get ready
```

`value` is `Just "Hello"`. Starting another attempt or reporting a failure drops it.

Develop with `nix develop`, then `./format.sh` and `./check.sh`. `nix build` runs the checks in a sandbox.

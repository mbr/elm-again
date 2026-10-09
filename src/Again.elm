module Again exposing (wakeAfter)

{-| Schedule retry wakeups.

@docs wakeAfter

-}

import Process
import Task


{-| Schedules a wakeup message after a delay in milliseconds.

`Nothing` produces `Cmd.none`.

-}
wakeAfter : msg -> Maybe Float -> Cmd msg
wakeAfter message delay =
    delay
        |> Maybe.map
            (\milliseconds ->
                Process.sleep milliseconds
                    |> Task.perform (always message)
            )
        |> Maybe.withDefault Cmd.none

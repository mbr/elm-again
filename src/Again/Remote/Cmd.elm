module Again.Remote.Cmd exposing (fail, wakeAfter)

{-| A helper module for using `Again.Remote` in your `update` function.

@docs fail, wakeAfter

-}

import Again.Remote as Remote exposing (Remote)
import Process
import Task


{-| Tell the `Remote` that an attempt failed and schedule your wakeup message
for the next retry. You get `Cmd.none` when no retry is allowed.
-}
fail : msg -> error -> Remote error value -> ( Remote error value, Cmd msg )
fail wakeup error remote =
    Remote.fail error remote
        |> Tuple.mapSecond (wakeAfter wakeup)


{-| Send a message after the given delay in milliseconds. With `Nothing`, there
is no message to schedule.
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

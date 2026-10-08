module Again.Task exposing (sleepUntil)

{-| Optional task adapters for interpreting retry deadlines outside the pure core.

@docs sleepUntil

-}

import Process
import Task exposing (Task)
import Time exposing (Posix)


{-| Sleeps until the deadline, returning immediately if it has passed. The delay
is calculated when the task runs. Scheduling may cause it to finish late, and
wall-clock changes during the sleep do not adjust the scheduled delay.

The unrestricted error type allows composition with tasks using any error type.

-}
sleepUntil : Posix -> Task x ()
sleepUntil deadline =
    Time.now
        |> Task.andThen
            (\now ->
                let
                    remaining =
                        Time.posixToMillis deadline - Time.posixToMillis now
                in
                if remaining <= 0 then
                    Task.succeed ()

                else
                    Process.sleep (toFloat remaining)
            )

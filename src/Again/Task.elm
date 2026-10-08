module Again.Task exposing (retry, retryIf)

{-| Run tasks with a retry policy. The first attempt runs immediately; the
schedule controls the waits between subsequent attempts.

    request
        |> retryIf isTransient policy
        |> Task.attempt ReceivedResponse

@docs retry, retryIf

-}

import Again.Policy exposing (AttemptLimit(..), Policy)
import Again.Schedule as Schedule
import Process
import Task exposing (Task)


{-| Retries any failure until the task succeeds or the attempt limit is reached.
Equivalent to `retryIf (always True)`.
-}
retry : Policy -> Task error value -> Task error value
retry =
    retryIf (always True)


{-| Retries only errors accepted by the predicate. Returns the last error when
it is rejected or the attempt limit is reached. Each execution starts a fresh
attempt count.
-}
retryIf : (error -> Bool) -> Policy -> Task error value -> Task error value
retryIf retryable policy task =
    attempt retryable policy task 1


{-| Runs one attempt, waiting before another if its error and the policy allow it.
-}
attempt : (error -> Bool) -> Policy -> Task error value -> Int -> Task error value
attempt retryable policy task count =
    task
        |> Task.onError
            (\error ->
                let
                    allowed =
                        case policy.limit of
                            Unlimited ->
                                True

                            MaxAttempts limit ->
                                count < limit
                in
                if retryable error && allowed then
                    Process.sleep (Schedule.delay policy.schedule (count - 1))
                        |> Task.andThen (\_ -> attempt retryable policy task (count + 1))

                else
                    Task.fail error
            )

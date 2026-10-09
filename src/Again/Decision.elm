module Again.Decision exposing (Decision(..), shouldRetry, retryDelay)

{-| Decide whether to retry a failure, optionally requesting a minimum wait.

@docs Decision, shouldRetry, retryDelay

-}

import Again.Policy as Policy exposing (Policy)


{-| Describes a decision made to retry an action.

`Stop` returns the error.

`Retry` uses the policy's delay.

`RetryAfter` waits at least the given number of milliseconds, using a finite,
nonnegative value.

Both retry decisions remain subject to the policy's attempt limit.

-}
type Decision
    = Stop
    | Retry
    | RetryAfter Float


{-| Whether the decision requests a retry, independent of policy limits.
-}
shouldRetry : Decision -> Bool
shouldRetry decision =
    case decision of
        Stop ->
            False

        Retry ->
            True

        RetryAfter _ ->
            True


{-| Applies the decision to the policy's next delay. Count attempts already made,
starting at one. Returns `Nothing` when stopped or exhausted.
-}
retryDelay : Decision -> Policy -> Int -> Maybe Float
retryDelay decision policy attempts =
    case decision of
        Stop ->
            Nothing

        Retry ->
            Policy.retryDelay policy attempts

        RetryAfter minimum ->
            Policy.retryDelay policy attempts
                |> Maybe.map (max minimum)

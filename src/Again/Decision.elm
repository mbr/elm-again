module Again.Decision exposing (Decision(..), fromBool, shouldRetry, retryDelay)

{-| Decide whether to retry a failure, optionally requesting a minimum wait.

@docs Decision, fromBool, shouldRetry, retryDelay

-}

import Again.Policy as Policy exposing (Policy)


{-| Describes a decision made to retry an action.

`Stop` returns the error.

`Retry` uses an unspecified delay.

`RetryAfter` waits at least the given number of milliseconds, using a finite,
nonnegative value.

-}
type Decision
    = Stop
    | Retry
    | RetryAfter Float


{-| Converts `True` to `Retry` and `False` to `Stop`.
-}
fromBool : Bool -> Decision
fromBool retryable =
    if retryable then
        Retry

    else
        Stop


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

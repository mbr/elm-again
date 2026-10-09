module Again.Decision exposing (Decision(..), fromBool, shouldRetry, retryDelay)

{-| Describe whether to retry an action and an optional minimum wait.

@docs Decision, fromBool, shouldRetry, retryDelay

-}

import Again.Policy as Policy exposing (Policy)


{-| Describes a decision made to retry an action.

`Stop` requests no further attempt.

`Retry` requests another attempt with an unspecified delay.

`RetryAfter` requests another attempt after at least the given number of
milliseconds. The delay must be finite and nonnegative.

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


{-| Whether the decision requests another attempt.
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


{-| Calculates the next delay in milliseconds. Count attempts already made,
starting at one. Returns `Nothing` when another attempt is not permitted.
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

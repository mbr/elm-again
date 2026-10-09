module Again.Policy exposing (Policy, AttemptLimit(..), unlimited, allowsRetry, retryDelay)

{-| Choose a retry schedule and how many attempts to allow.

@docs Policy, AttemptLimit, unlimited, allowsRetry, retryDelay

-}

import Again.Schedule as Schedule exposing (Schedule)


{-| A retry schedule and an attempt limit.
-}
type alias Policy =
    { schedule : Schedule
    , limit : AttemptLimit
    }


{-| `Unlimited` keeps offering retries.

`MaxAttempts` counts the initial operation as an attempt, so `MaxAttempts 3`
allows two retries. A limit of one or less allows no retries.

-}
type AttemptLimit
    = Unlimited
    | MaxAttempts Int


{-| Uses a schedule with no attempt limit.
-}
unlimited : Schedule -> Policy
unlimited schedule =
    { schedule = schedule, limit = Unlimited }


{-| Whether another attempt is allowed. The count includes the initial operation
and any retries already made.
-}
allowsRetry : Policy -> Int -> Bool
allowsRetry policy attempts =
    case policy.limit of
        Unlimited ->
            True

        MaxAttempts limit ->
            attempts < limit


{-| Delay in milliseconds before another attempt, or `Nothing` when exhausted.
Count attempts already made, starting at one for the initial operation.
-}
retryDelay : Policy -> Int -> Maybe Float
retryDelay policy attempts =
    if allowsRetry policy attempts then
        Just (Schedule.delay policy.schedule (attempts - 1))

    else
        Nothing

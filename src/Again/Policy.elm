module Again.Policy exposing (Policy, AttemptLimit(..), init, allowsRetry)

{-| Choose a retry schedule and how many attempts to allow.

@docs Policy, AttemptLimit, init, allowsRetry

-}

import Again.Schedule exposing (Schedule)


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
init : Schedule -> Policy
init schedule =
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

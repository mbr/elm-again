module Again.Policy exposing (Policy, AttemptLimit(..), init, withMaxAttempts)

{-| Choose a retry schedule and how many attempts to allow.

@docs Policy, AttemptLimit, init, withMaxAttempts

-}

import Again.Schedule exposing (Schedule)


{-| A retry schedule and an attempt limit, ready to pass to `Again.init`.
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


{-| Sets the attempt limit while keeping the schedule.
-}
withMaxAttempts : Int -> Policy -> Policy
withMaxAttempts limit policy =
    { policy | limit = MaxAttempts limit }

module Again exposing (Retry, init, succeeded, failed, failures)

{-| Count failures and obtain retry delays. Callers decide which failures warrant
retrying and handle all waiting and execution.

@docs Retry, init, succeeded, failed, failures

-}

import Again.Policy as Policy exposing (Policy)
import Again.Schedule as Schedule


{-| A policy and its failure count since initialization or the last success.
-}
type Retry
    = Retry
        { policy : Policy
        , failures : Int
        }


{-| Starts a retry sequence using a policy, without performing an attempt.
-}
init : Policy -> Retry
init policy =
    Retry { policy = policy, failures = 0 }


{-| Resets the failure count while retaining the policy.
-}
succeeded : Retry -> Retry
succeeded (Retry state) =
    Retry { state | failures = 0 }


{-| Records a failure and returns the updated retry state and a delay in
milliseconds, or `Nothing` when the attempt limit is reached. The first failure
uses retry index zero. Every call counts as a failure, even after the limit.
-}
failed : Retry -> ( Retry, Maybe Float )
failed (Retry state) =
    let
        count =
            state.failures + 1

        limitReached =
            case state.policy.limit of
                Policy.Unlimited ->
                    False

                Policy.MaxAttempts limit ->
                    count >= limit
    in
    ( Retry { state | failures = count }
    , if limitReached then
        Nothing

      else
        Just (Schedule.delay state.policy.schedule state.failures)
    )


{-| Counts failures since initialization or the last success, not attempts run.
-}
failures : Retry -> Int
failures (Retry state) =
    state.failures

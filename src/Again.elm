module Again exposing (Retry, init, succeeded, failed, failures)

{-| Count failures and obtain retry delays. Callers decide which failures warrant
retrying and handle all waiting and execution.

@docs Retry, init, succeeded, failed, failures

-}

import Again.Policy as Policy exposing (Policy)


{-| A policy and its failure count since initialization or the last success.
-}
type Retry
    = Retry
        { policy : Policy
        , failures : Int
        }


{-| Starts a retry sequence without performing an attempt.
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
milliseconds. The first failure uses retry index zero.
-}
failed : Retry -> ( Retry, Float )
failed (Retry state) =
    ( Retry { state | failures = state.failures + 1 }
    , Policy.delay state.policy state.failures
    )


{-| Counts failures since initialization or the last success, not attempts run.
-}
failures : Retry -> Int
failures (Retry state) =
    state.failures

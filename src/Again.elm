module Again exposing (Retry, init, withMaxAttempts, succeeded, failed, failures)

{-| Count failures and obtain retry delays. Callers decide which failures warrant
retrying and handle all waiting and execution.

@docs Retry, init, withMaxAttempts, succeeded, failed, failures

-}

import Again.Policy as Policy exposing (Policy)


{-| A policy, an optional attempt limit, and a failure count since initialization
or the last success.
-}
type Retry
    = Retry
        { policy : Policy
        , failures : Int
        , maxAttempts : Maybe Int
        }


{-| Starts an unlimited retry sequence without performing an attempt.
-}
init : Policy -> Retry
init policy =
    Retry { policy = policy, failures = 0, maxAttempts = Nothing }


{-| Limits attempts, including the initial operation. For example, a limit of
three allows two retries. A limit of one or less allows no retries.
Setting a limit preserves the current failure count.
-}
withMaxAttempts : Int -> Retry -> Retry
withMaxAttempts limit (Retry state) =
    Retry { state | maxAttempts = Just limit }


{-| Resets the failure count while retaining the policy and attempt limit.
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
            state.maxAttempts
                |> Maybe.map (\limit -> count >= limit)
                |> Maybe.withDefault False
    in
    ( Retry { state | failures = count }
    , if limitReached then
        Nothing

      else
        Just (Policy.delay state.policy state.failures)
    )


{-| Counts failures since initialization or the last success, not attempts run.
-}
failures : Retry -> Int
failures (Retry state) =
    state.failures

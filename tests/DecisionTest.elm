module DecisionTest exposing (tests)

{-| Checks retry decisions against schedule delays and attempt limits.
-}

import Again.Decision as Decision exposing (Decision(..))
import Again.Policy as Policy
import Again.Schedule exposing (Schedule(..))
import Expect
import Test exposing (Test, describe, test)


{-| Covers stopping, minimum waits, and exhaustion without running tasks.
-}
tests : Test
tests =
    describe "Again.Decision"
        [ test "only Stop rejects a retry regardless of its delay" <|
            \_ ->
                [ Stop, Retry, RetryAfter 0, RetryAfter 5000 ]
                    |> List.map Decision.shouldRetry
                    |> Expect.equal [ False, True, True, True ]
        , test "minimum waits preserve backoff and all decisions respect attempt limits" <|
            \_ ->
                let
                    policy =
                        { schedule = ExponentialBackoff { initialDelay = 1000, multiplier = 2, maxDelay = 4000 }
                        , limit = Policy.MaxAttempts 4
                        }
                in
                [ Stop, Retry, RetryAfter 0, RetryAfter 1500, RetryAfter 5000 ]
                    |> List.map
                        (\decision ->
                            List.range 1 4
                                |> List.map (Decision.retryDelay decision policy)
                        )
                    |> Expect.equal
                        [ List.repeat 4 Nothing
                        , [ Just 1000, Just 2000, Just 4000, Nothing ]
                        , [ Just 1000, Just 2000, Just 4000, Nothing ]
                        , [ Just 1500, Just 2000, Just 4000, Nothing ]
                        , [ Just 5000, Just 5000, Just 5000, Nothing ]
                        ]
        ]

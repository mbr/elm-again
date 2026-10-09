module PolicyTest exposing (tests)

{-| Checks retry delays and attempt-limit boundaries.
-}

import Again.Policy as Policy exposing (AttemptLimit(..))
import Again.Schedule as Schedule
import Expect
import Test exposing (Test, describe, test)


{-| Verifies delay indexing and limits that include the initial operation.
-}
tests : Test
tests =
    describe "Again.Policy"
        [ test "the default policy permits retries regardless of attempts already made" <|
            \_ ->
                [ 1, 2, 100000 ]
                    |> List.map (Policy.allowsRetry (Policy.init Schedule.immediately))
                    |> Expect.equal [ True, True, True ]
        , test "three attempts permit two retries; limits of one or less permit none" <|
            \_ ->
                [ MaxAttempts 3, MaxAttempts 1, MaxAttempts 0, MaxAttempts -1 ]
                    |> List.map
                        (\limit ->
                            [ 1, 2, 3, 4 ]
                                |> List.map
                                    (Policy.allowsRetry
                                        { schedule = Schedule.immediately, limit = limit }
                                    )
                        )
                    |> Expect.equal
                        [ [ True, True, False, False ]
                        , [ False, False, False, False ]
                        , [ False, False, False, False ]
                        , [ False, False, False, False ]
                        ]
        , test "retry delays start at the first schedule entry and stop at the attempt limit" <|
            \_ ->
                let
                    schedule =
                        Schedule.ImmediatelyThen
                            (Schedule.ExponentialBackoff
                                { initialDelay = 1000, multiplier = 2, maxDelay = 4000 }
                            )
                in
                [ Unlimited, MaxAttempts 6, MaxAttempts 1, MaxAttempts 0, MaxAttempts -1 ]
                    |> List.map
                        (\limit ->
                            List.range 1 7
                                |> List.map (Policy.retryDelay { schedule = schedule, limit = limit })
                        )
                    |> Expect.equal
                        [ [ Just 0, Just 1000, Just 2000, Just 4000, Just 4000, Just 4000, Just 4000 ]
                        , [ Just 0, Just 1000, Just 2000, Just 4000, Just 4000, Nothing, Nothing ]
                        , List.repeat 7 Nothing
                        , List.repeat 7 Nothing
                        , List.repeat 7 Nothing
                        ]
        ]

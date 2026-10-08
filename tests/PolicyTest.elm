module PolicyTest exposing (tests)

{-| Checks retry permission at attempt-limit boundaries.
-}

import Again.Policy as Policy exposing (AttemptLimit(..))
import Again.Schedule as Schedule
import Expect
import Test exposing (Test, describe, test)


{-| Verifies that limits include the initial operation and remain exhausted.
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
        ]

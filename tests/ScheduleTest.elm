module ScheduleTest exposing (tests)

{-| Checks delay sequences, schedule composition, and numeric boundaries.
-}

import Again.Schedule as Schedule exposing (Schedule(..))
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz2, test)


{-| Exercises retry schedules.
-}
tests : Test
tests =
    describe "Again.Schedule"
        [ test "calculates immediate, periodic, and capped exponential delays with fractional milliseconds" <|
            \_ ->
                let
                    periodic =
                        Schedule.delay (Periodic { delay = 0.25 })

                    exponential =
                        Schedule.delay
                            (ExponentialBackoff
                                { initialDelay = 0.25, multiplier = 2, maxDelay = 1.5 }
                            )
                in
                Expect.equal
                    ( [ 0, 0, 0, 0, 0 ], [ 0.25, 0.25, 0.25, 0.25, 0.25 ], [ 0.25, 0.5, 1, 1.5, 1.5 ] )
                    ( List.map (Schedule.delay Schedule.immediately) (List.range 0 4)
                    , List.map periodic (List.range 0 4)
                    , List.map exponential (List.range 0 4)
                    )
        , test "nested immediate schedules each prepend exactly one zero delay" <|
            \_ ->
                List.range 0 4
                    |> List.map (Schedule.delay (ImmediatelyThen (ImmediatelyThen (Periodic { delay = 1000 }))))
                    |> Expect.equal [ 0, 0, 1000, 1000, 1000 ]
        , test "large retry indices preserve zero delays, constant growth, and the cap" <|
            \_ ->
                [ Schedule.immediately
                , ExponentialBackoff { initialDelay = 0, multiplier = 2, maxDelay = 1000 }
                , ExponentialBackoff { initialDelay = 0, multiplier = 2, maxDelay = 0 }
                , ExponentialBackoff { initialDelay = 0.25, multiplier = 1, maxDelay = 1000 }
                , ExponentialBackoff { initialDelay = 0.25, multiplier = 2, maxDelay = 1000 }
                ]
                    |> List.map (\schedule -> Schedule.delay schedule 100000)
                    |> Expect.equal [ 0, 0, 0, 0.25, 1000 ]
        , fuzz2 (Fuzz.intRange 0 10000) (Fuzz.intRange 0 10000) "an immediate prefix shifts the wrapped sequence without changing its delays" <|
            \initialDelay retryIndex ->
                let
                    inner =
                        ExponentialBackoff
                            { initialDelay = toFloat initialDelay
                            , multiplier = 2
                            , maxDelay = 10000
                            }

                    wrapped =
                        ImmediatelyThen inner
                in
                Expect.equal
                    ( 0, Schedule.delay inner retryIndex )
                    ( Schedule.delay wrapped 0, Schedule.delay wrapped (retryIndex + 1) )
        ]

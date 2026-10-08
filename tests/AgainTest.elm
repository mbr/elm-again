module AgainTest exposing (tests)

{-| Checks failure counting, policy progression, and success resets.
-}

import Again
import Again.Policy exposing (Policy(..))
import Expect
import Test exposing (Test, describe, test)


{-| Exercises retry sequences without clocks or effects.
-}
tests : Test
tests =
    describe "Again"
        [ test "counts failures, advances backoff, and restarts the same policy after success" <|
            \_ ->
                let
                    initial =
                        Again.init
                            (ImmediatelyThen
                                (ExponentialBackoff
                                    { initialDelay = 1000, multiplier = 2, maxDelay = 3000 }
                                )
                            )

                    ( first, firstDelay ) =
                        Again.failed initial

                    ( second, secondDelay ) =
                        Again.failed first

                    ( third, thirdDelay ) =
                        Again.failed second

                    ( fourth, fourthDelay ) =
                        Again.failed third

                    ready =
                        Again.succeeded fourth

                    ( restarted, restartDelay ) =
                        Again.failed ready

                    ( following, followingDelay ) =
                        Again.failed restarted
                in
                Expect.all
                    [ \_ ->
                        List.map Again.failures [ initial, first, second, third, fourth, ready, restarted, following ]
                            |> Expect.equal [ 0, 1, 2, 3, 4, 0, 1, 2 ]
                    , \_ ->
                        [ firstDelay, secondDelay, thirdDelay, fourthDelay, restartDelay, followingDelay ]
                            |> Expect.equal (List.map Just [ 0, 1000, 2000, 3000, 0, 1000 ])
                    ]
                    ()
        , test "success before any failure leaves the first periodic delay intact" <|
            \_ ->
                let
                    initial =
                        Again.init (Periodic { delay = 0.25 })
                            |> Again.succeeded
                            |> Again.succeeded

                    ( retry, wait ) =
                        Again.failed initial
                in
                Expect.equal ( 0, 1, Just 0.25 ) ( Again.failures initial, Again.failures retry, wait )
        , test "limits include the initial attempt, count exhausted failures, and survive success" <|
            \_ ->
                let
                    initial =
                        Again.init (ImmediatelyThen (Periodic { delay = 1000 }))
                            |> Again.withMaxAttempts 3

                    ( exhausted, delays ) =
                        failSeveral 4 initial

                    ready =
                        Again.succeeded exhausted

                    ( exhaustedAgain, restartedDelays ) =
                        failSeveral 3 ready
                in
                Expect.all
                    [ \_ -> Expect.equal [ Just 0, Just 1000, Nothing, Nothing ] delays
                    , \_ -> Expect.equal [ Just 0, Just 1000, Nothing ] restartedDelays
                    , \_ ->
                        List.map Again.failures [ initial, exhausted, ready, exhaustedAgain ]
                            |> Expect.equal [ 0, 4, 0, 3 ]
                    ]
                    ()
        , test "limits of one or less never offer a retry, even with an immediate policy" <|
            \_ ->
                [ -1, 0, 1 ]
                    |> List.map
                        (\limit ->
                            Again.init (ImmediatelyThen (Periodic { delay = 1000 }))
                                |> Again.withMaxAttempts limit
                                |> failSeveral 2
                                |> Tuple.mapFirst Again.failures
                        )
                    |> Expect.equal (List.repeat 3 ( 2, [ Nothing, Nothing ] ))
        , test "changing the limit preserves failures and the policy's position" <|
            \_ ->
                let
                    ( retry, _ ) =
                        Again.init
                            (ExponentialBackoff { initialDelay = 1000, multiplier = 2, maxDelay = 10000 })
                            |> failSeveral 2

                    limited =
                        Again.withMaxAttempts 3 retry

                    ( stopped, stoppedDelay ) =
                        Again.failed limited

                    ( continued, continuedDelay ) =
                        limited
                            |> Again.withMaxAttempts 4
                            |> Again.failed
                in
                Expect.equal
                    ( [ 2, 3, 3 ], [ Nothing, Just 4000 ] )
                    ( List.map Again.failures [ limited, stopped, continued ], [ stoppedDelay, continuedDelay ] )
        ]


{-| Records consecutive failures and collects their retry delays.
-}
failSeveral : Int -> Again.Retry -> ( Again.Retry, List (Maybe Float) )
failSeveral count retry =
    if count <= 0 then
        ( retry, [] )

    else
        let
            ( next, delay ) =
                Again.failed retry

            ( final, remainingDelays ) =
                failSeveral (count - 1) next
        in
        ( final, delay :: remainingDelays )

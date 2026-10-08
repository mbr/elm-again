module AgainTest exposing (tests)

{-| Checks failure counting, policy progression, and success resets.
-}

import Again
import Again.Policy as Policy
import Again.Schedule exposing (Schedule(..))
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
                        ImmediatelyThen
                            (ExponentialBackoff
                                { initialDelay = 1000, multiplier = 2, maxDelay = 3000 }
                            )
                            |> Policy.init
                            |> Again.init

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
                        Periodic { delay = 0.25 }
                            |> Policy.init
                            |> Again.init
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
                        ImmediatelyThen (Periodic { delay = 1000 })
                            |> Policy.init
                            |> Policy.withMaxAttempts 3
                            |> Again.init

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
                            Again.init
                                { schedule = ImmediatelyThen (Periodic { delay = 1000 })
                                , limit = Policy.MaxAttempts limit
                                }
                                |> failSeveral 2
                                |> Tuple.mapFirst Again.failures
                        )
                    |> Expect.equal (List.repeat 3 ( 2, [ Nothing, Nothing ] ))
        , test "overriding a policy's limit preserves its schedule" <|
            \_ ->
                let
                    limited =
                        ExponentialBackoff { initialDelay = 1000, multiplier = 2, maxDelay = 10000 }
                            |> Policy.init
                            |> Policy.withMaxAttempts 3

                    ( stopped, delays ) =
                        limited
                            |> Again.init
                            |> failSeveral 4

                    ( stoppedLater, extendedDelays ) =
                        limited
                            |> Policy.withMaxAttempts 4
                            |> Again.init
                            |> failSeveral 4
                in
                Expect.all
                    [ \_ -> Expect.equal [ Just 1000, Just 2000, Nothing, Nothing ] delays
                    , \_ -> Expect.equal [ Just 1000, Just 2000, Just 4000, Nothing ] extendedDelays
                    , \_ -> Expect.equal ( 4, 4 ) ( Again.failures stopped, Again.failures stoppedLater )
                    ]
                    ()
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

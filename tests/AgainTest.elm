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
                            |> Expect.equal [ 0, 1000, 2000, 3000, 0, 1000 ]
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
                Expect.equal ( 0, 1, 0.25 ) ( Again.failures initial, Again.failures retry, wait )
        ]

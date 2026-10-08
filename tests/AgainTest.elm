module AgainTest exposing (tests)

{-| Exercises the transport-independent retry lifecycle and policy boundaries.
-}

import Again exposing (Action(..), Event(..), Status(..))
import Expect
import Fuzz
import Random
import Test exposing (Test, describe, fuzz, test)
import Time


{-| Runs retry scenarios using an explicit clock and deterministic randomness.
-}
tests : Test
tests =
    describe "Again"
        [ test "retries with capped delays, ignores stale input, and retains successful data" <|
            \_ ->
                started (Again.exponential { initialDelay = 100, multiplier = 2, maxDelay = 150 }) <|
                    \first running ->
                        let
                            ( waiting, wake ) =
                                Again.update (at 0) (Failed first "offline") running

                            ( early, earlyActions ) =
                                Again.update (at 99) TimerElapsed waiting
                        in
                        case Again.update (at 100) TimerElapsed early of
                            ( retrying, [ Run second ] ) ->
                                let
                                    ( stale, staleActions ) =
                                        Again.update (at 101) (Succeeded first "old") retrying

                                    ( waitingAgain, nextWake ) =
                                        Again.update (at 102) (Failed second "still offline") stale
                                in
                                case Again.update (at 252) TimerElapsed waitingAgain of
                                    ( thirdRunning, [ Run third ] ) ->
                                        let
                                            ( ready, _ ) =
                                                Again.update (at 253) (Succeeded third "cached") thirdRunning

                                            ( invalidated, invalidationActions ) =
                                                Again.update (at 254) (Invalidated "lost") ready
                                        in
                                        Expect.all
                                            [ \_ -> Expect.equal [ WakeAt (at 100) ] wake
                                            , \_ -> Expect.equal [] earlyActions
                                            , \_ -> Expect.equal [] staleActions
                                            , \_ -> Expect.equal Nothing (Again.value stale)
                                            , \_ -> Expect.equal [ WakeAt (at 252) ] nextWake
                                            , \_ -> Expect.equal Ready (Again.status ready)
                                            , \_ -> Expect.equal Nothing (Again.lastError ready)
                                            , \_ -> Expect.equal (Just "cached") (Again.value invalidated)
                                            , \_ -> Expect.equal (Just "lost") (Again.lastError invalidated)
                                            , \_ -> Expect.equal [ WakeAt (at 354) ] invalidationActions
                                            , \_ -> Expect.equal [] (Again.update (at 300) TimerElapsed ready |> Tuple.second)
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "expected third attempt"

                            _ ->
                                Expect.fail "expected second attempt"
        , test "limits include the first attempt and explicit restart uses a new identity" <|
            \_ ->
                started (Again.constant 10 |> Again.withMaxAttempts 1) <|
                    \first running ->
                        let
                            ( exhausted, actions ) =
                                Again.update (at 0) (Failed first "failed") running
                        in
                        case Again.update (at 1) Start exhausted of
                            ( restarted, [ Run next ] ) ->
                                Expect.all
                                    [ \_ -> Expect.equal Exhausted (Again.status exhausted)
                                    , \_ -> Expect.equal [] actions
                                    , \_ -> Expect.equal 1 (Again.attempts restarted)
                                    , \_ -> Expect.notEqual first next
                                    , \_ -> Expect.equal [] (Again.update (at 2) (Failed first "stale") restarted |> Tuple.second)
                                    ]
                                    ()

                            _ ->
                                Expect.fail "expected restarted execution"
        , test "flapping preserves attempts until the configured stable duration" <|
            \_ ->
                started (Again.constant 10 |> Again.withMaxAttempts 1 |> Again.withResetAfter 60) <|
                    \first running ->
                        let
                            ( ready, _ ) =
                                Again.update (at 10) (Succeeded first ()) running

                            ( flapping, _ ) =
                                Again.update (at 69) (Invalidated "lost") ready

                            ( stable, actions ) =
                                Again.update (at 70) (Invalidated "lost") ready
                        in
                        Expect.all
                            [ \_ -> Expect.equal Exhausted (Again.status flapping)
                            , \_ -> Expect.equal (Just ()) (Again.value flapping)
                            , \_ -> Expect.equal 0 (Again.attempts stable)
                            , \_ -> Expect.equal [ WakeAt (at 80) ] actions
                            ]
                            ()
        , fuzz Fuzz.int "jitter is deterministic and stays within its configured bounds" <|
            \seed ->
                let
                    policy =
                        Again.exponential { initialDelay = 1000, multiplier = 2, maxDelay = 3000 }
                            |> Again.withJitter 0.2

                    initial =
                        Again.init policy (Random.initialSeed seed)
                in
                case Again.update (at 0) Start initial of
                    ( running, [ Run first ] ) ->
                        let
                            result =
                                Again.update (at 0) (Failed first ()) running
                        in
                        case Tuple.second result of
                            [ WakeAt deadline ] ->
                                Expect.all
                                    [ \_ -> Expect.atLeast 800 (Time.posixToMillis deadline)
                                    , \_ -> Expect.atMost 1200 (Time.posixToMillis deadline)
                                    , \_ -> Expect.equal (Tuple.second result) (Again.update (at 0) (Failed first ()) running |> Tuple.second)
                                    ]
                                    ()

                            _ ->
                                Expect.fail "expected jittered wakeup"

                    _ ->
                        Expect.fail "expected initial execution"
        ]


{-| Begins an execution and passes its opaque identity to a scenario.
-}
started : Again.Policy -> (Again.AttemptId -> Again.Model error value -> Expect.Expectation) -> Expect.Expectation
started policy scenario =
    case Again.update (at 0) Start (Again.init policy (Random.initialSeed 17)) of
        ( model, [ Run attempt ] ) ->
            scenario attempt model

        _ ->
            Expect.fail "expected initial execution"


{-| Constructs an explicit test clock reading.
-}
at : Int -> Time.Posix
at =
    Time.millisToPosix

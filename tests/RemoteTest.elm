module RemoteTest exposing (tests)

{-| Checks manual retries, loss of successful values, and accepted recovery.
-}

import Again.Decision exposing (Decision(..))
import Again.Policy as Policy
import Again.Remote as Remote exposing (State(..))
import Again.Schedule exposing (Schedule(..))
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


{-| Makes retry indexing observable within a bounded sequence.
-}
policy : Policy.Policy
policy =
    { schedule = ExponentialBackoff { initialDelay = 1000, multiplier = 2, maxDelay = 4000 }
    , limit = Policy.MaxAttempts 3
    }


{-| Covers lifecycle transitions, terminal decisions, and counter boundaries.
-}
tests : Test
tests =
    describe "Again.Remote"
        [ test "success after retries can be lost and recovered without retaining the old value" <|
            \_ ->
                let
                    initial =
                        Remote.init policy

                    connecting =
                        Remote.started initial

                    ( waiting, firstDelay ) =
                        Remote.failed "timeout" connecting

                    retrying =
                        Remote.started waiting

                    ( waitingAgain, secondDelay ) =
                        Remote.failed "refused" retrying

                    connected =
                        waitingAgain |> Remote.started |> Remote.ok "socket"

                    ( lost, resetDelay ) =
                        Remote.failed "closed" connected

                    recovered =
                        Remote.ok "replacement" lost

                    states =
                        [ initial, connecting, waiting, retrying, waitingAgain, connected, lost, recovered ]
                in
                Expect.all
                    [ \_ ->
                        List.map Remote.state states
                            |> Expect.equal
                                [ NotAttempted
                                , Attempting
                                , WaitingForRetry { attempts = 1, lastError = "timeout" }
                                , Retrying { attempts = 1, lastError = "timeout" }
                                , WaitingForRetry { attempts = 2, lastError = "refused" }
                                , Successful "socket"
                                , WaitingForRetry { attempts = 1, lastError = "closed" }
                                , Successful "replacement"
                                ]
                    , \_ -> Expect.equal [ Just 1000, Just 2000, Just 1000 ] [ firstDelay, secondDelay, resetDelay ]
                    , \_ ->
                        List.map Remote.result states
                            |> Expect.equal [ Nothing, Nothing, Nothing, Nothing, Nothing, Just (Ok "socket"), Nothing, Just (Ok "replacement") ]
                    , \_ ->
                        List.map Remote.get states
                            |> Expect.equal [ Nothing, Nothing, Nothing, Nothing, Nothing, Just "socket", Nothing, Just "replacement" ]
                    ]
                    ()
        , fuzz (Fuzz.intRange -2 12) "attempt limits include the initial attempt and exhaustion exposes the final error" <|
            \limit ->
                let
                    attempts =
                        max 1 limit

                    ( exhausted, delays ) =
                        List.range 1 attempts
                            |> List.foldl
                                (\number ( current, previousDelays ) ->
                                    let
                                        ( next, delay ) =
                                            current |> Remote.started |> Remote.failed (String.fromInt number)
                                    in
                                    ( next, previousDelays ++ [ delay ] )
                                )
                                ( Remote.init { schedule = Periodic { delay = 1000 }, limit = Policy.MaxAttempts limit }, [] )
                in
                Expect.all
                    [ \_ -> Expect.equal (List.repeat (attempts - 1) (Just 1000) ++ [ Nothing ]) delays
                    , \_ -> Expect.equal (Failed (String.fromInt attempts)) (Remote.state exhausted)
                    , \_ -> Expect.equal (Just (Err (String.fromInt attempts))) (Remote.result exhausted)
                    , \_ -> Expect.equal Nothing (Remote.get exhausted)
                    ]
                    ()
        , test "starts and failures outside their active states do nothing, while accepted successes replace any state" <|
            \_ ->
                let
                    initial =
                        Remote.init policy

                    attempting =
                        Remote.started initial

                    waiting =
                        Remote.failed "timeout" attempting |> Tuple.first

                    retrying =
                        Remote.started waiting

                    successful =
                        Remote.ok "socket" retrying

                    stopped =
                        Remote.failedWith Stop "denied" attempting |> Tuple.first

                    unchangedStarts =
                        [ attempting, retrying, successful, stopped ]

                    unchangedFailures =
                        [ initial, waiting, stopped ]

                    allStates =
                        [ initial, attempting, waiting, retrying, successful, stopped ]
                in
                Expect.all
                    [ \_ -> Expect.equal unchangedStarts (List.map Remote.started unchangedStarts)
                    , \_ ->
                        List.map (Remote.failed "duplicate") unchangedFailures
                            |> Expect.equal (List.map (\current -> ( current, Nothing )) unchangedFailures)
                    , \_ ->
                        List.map (Remote.failedWith Stop "duplicate") unchangedFailures
                            |> Expect.equal (List.map (\current -> ( current, Nothing )) unchangedFailures)
                    , \_ ->
                        List.map (Remote.ok "replacement") allStates
                            |> Expect.equal (List.repeat 6 (Remote.init policy |> Remote.ok "replacement"))
                    ]
                    ()
        , test "decisions can stop or extend a wait but cannot exceed the attempt limit" <|
            \_ ->
                let
                    attempting =
                        Remote.init policy |> Remote.started

                    ( waiting, firstDelay ) =
                        Remote.failedWith (RetryAfter 1500) "busy" attempting

                    ( waitingAgain, secondDelay ) =
                        waiting |> Remote.started |> Remote.failedWith (RetryAfter 10) "still busy"

                    exhausted =
                        waitingAgain |> Remote.started |> Remote.failedWith (RetryAfter 5000) "unavailable"

                    stopped =
                        Remote.failedWith Stop "denied" attempting
                in
                [ ( waiting, firstDelay ), ( waitingAgain, secondDelay ), exhausted, stopped ]
                    |> List.map (Tuple.mapFirst Remote.state)
                    |> Expect.equal
                        [ ( WaitingForRetry { attempts = 1, lastError = "busy" }, Just 1500 )
                        , ( WaitingForRetry { attempts = 2, lastError = "still busy" }, Just 2000 )
                        , ( Failed "unavailable", Nothing )
                        , ( Failed "denied", Nothing )
                        ]
        ]

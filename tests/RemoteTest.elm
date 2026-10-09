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
                        Remote.notAttempted policy

                    connecting =
                        Remote.attempting policy

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
        , fuzz (Fuzz.pair (Fuzz.intRange -2 12) Fuzz.bool) "reported failures count toward limits with or without start notifications" <|
            \( limit, reportStarts ) ->
                let
                    attempts =
                        max 1 limit

                    ( exhausted, delays ) =
                        List.range 1 attempts
                            |> List.foldl
                                (\number ( current, previousDelays ) ->
                                    let
                                        ( next, delay ) =
                                            current
                                                |> (if reportStarts then
                                                        Remote.started

                                                    else
                                                        identity
                                                   )
                                                |> Remote.failed (String.fromInt number)
                                    in
                                    ( next, previousDelays ++ [ delay ] )
                                )
                                ( Remote.notAttempted { schedule = Periodic { delay = 1000 }, limit = Policy.MaxAttempts limit }, [] )
                in
                Expect.all
                    [ \_ -> Expect.equal (List.repeat (attempts - 1) (Just 1000) ++ [ Nothing ]) delays
                    , \_ -> Expect.equal (Failed (String.fromInt attempts)) (Remote.state exhausted)
                    , \_ -> Expect.equal (Just (Err (String.fromInt attempts))) (Remote.result exhausted)
                    , \_ -> Expect.equal Nothing (Remote.get exhausted)
                    ]
                    ()
        , test "starts and outcomes apply from every state, preserving only active retry context" <|
            \_ ->
                let
                    initial =
                        Remote.notAttempted policy

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

                    exhausted =
                        retrying
                            |> Remote.failed "refused"
                            |> Tuple.first
                            |> Remote.started
                            |> Remote.failed "unavailable"
                            |> Tuple.first

                    allStates =
                        [ initial, attempting, waiting, retrying, successful, stopped, exhausted ]
                in
                Expect.all
                    [ \_ ->
                        List.map (Remote.started >> Remote.state) allStates
                            |> Expect.equal
                                [ Attempting
                                , Attempting
                                , Retrying { attempts = 1, lastError = "timeout" }
                                , Retrying { attempts = 1, lastError = "timeout" }
                                , Attempting
                                , Attempting
                                , Attempting
                                ]
                    , \_ ->
                        List.map (Remote.failed "new failure" >> Tuple.mapFirst Remote.state) allStates
                            |> Expect.equal
                                [ ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 2, lastError = "new failure" }, Just 2000 )
                                , ( WaitingForRetry { attempts = 2, lastError = "new failure" }, Just 2000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                ]
                    , \_ ->
                        List.map (Remote.failedWith Stop "denied again" >> Tuple.mapFirst Remote.state) allStates
                            |> Expect.equal (List.repeat 7 ( Failed "denied again", Nothing ))
                    , \_ ->
                        List.map (Remote.ok "replacement") allStates
                            |> Expect.equal (List.repeat 7 (Remote.notAttempted policy |> Remote.ok "replacement"))
                    ]
                    ()
        , test "decisions can stop or extend a wait but cannot exceed the attempt limit" <|
            \_ ->
                let
                    attempting =
                        Remote.attempting policy

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

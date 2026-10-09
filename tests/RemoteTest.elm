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
                    connecting =
                        Remote.init policy

                    ( waiting, firstDelay ) =
                        Remote.failed "timeout" connecting

                    retrying =
                        Remote.retry waiting |> Tuple.first

                    ( waitingAgain, secondDelay ) =
                        Remote.failed "refused" retrying

                    connected =
                        waitingAgain |> Remote.retry |> Tuple.first |> Remote.ok "socket"

                    ( lost, resetDelay ) =
                        Remote.failed "closed" connected

                    recovered =
                        Remote.ok "replacement" lost

                    states =
                        [ connecting, waiting, retrying, waitingAgain, connected, lost, recovered ]
                in
                Expect.all
                    [ \_ ->
                        List.map Remote.state states
                            |> Expect.equal
                                [ Attempting
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
                            |> Expect.equal [ Nothing, Nothing, Nothing, Nothing, Just (Ok "socket"), Nothing, Just (Ok "replacement") ]
                    , \_ ->
                        List.map Remote.get states
                            |> Expect.equal [ Nothing, Nothing, Nothing, Nothing, Just "socket", Nothing, Just "replacement" ]
                    ]
                    ()
        , fuzz (Fuzz.pair (Fuzz.intRange -2 12) Fuzz.bool) "reported failures count toward limits with or without retry transitions" <|
            \( limit, reportRetries ) ->
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
                                                |> (if reportRetries then
                                                        Remote.retry >> Tuple.first

                                                    else
                                                        identity
                                                   )
                                                |> Remote.failed (String.fromInt number)
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
        , test "retries require a waiting state while outcomes can be recorded from any state" <|
            \_ ->
                let
                    attempting =
                        Remote.init policy

                    waiting =
                        Remote.failed "timeout" attempting |> Tuple.first

                    retrying =
                        Remote.retry waiting |> Tuple.first

                    successful =
                        Remote.ok "socket" retrying

                    rejected =
                        Remote.failedWith Stop "denied" attempting |> Tuple.first

                    exhausted =
                        retrying
                            |> Remote.failed "refused"
                            |> Tuple.first
                            |> Remote.retry
                            |> Tuple.first
                            |> Remote.failed "unavailable"
                            |> Tuple.first

                    allStates =
                        [ attempting, waiting, retrying, successful, rejected, exhausted ]
                in
                Expect.all
                    [ \_ ->
                        List.map Remote.retry allStates
                            |> Expect.equal
                                [ ( attempting, False )
                                , ( retrying, True )
                                , ( retrying, False )
                                , ( successful, False )
                                , ( rejected, False )
                                , ( exhausted, False )
                                ]
                    , \_ ->
                        List.map (Remote.failed "new failure" >> Tuple.mapFirst Remote.state) allStates
                            |> Expect.equal
                                [ ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 2, lastError = "new failure" }, Just 2000 )
                                , ( WaitingForRetry { attempts = 2, lastError = "new failure" }, Just 2000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                ]
                    , \_ ->
                        List.map (Remote.failedWith Stop "denied again" >> Tuple.mapFirst Remote.state) allStates
                            |> Expect.equal (List.repeat 6 ( Failed "denied again", Nothing ))
                    , \_ ->
                        List.map (Remote.ok "replacement") allStates
                            |> Expect.equal (List.repeat 6 (Remote.init policy |> Remote.ok "replacement"))
                    ]
                    ()
        , test "scheduled failures return updated state for retries and terminal errors" <|
            \_ ->
                let
                    attempting =
                        Remote.init policy

                    waiting =
                        Remote.failed "first failure" attempting |> Tuple.first

                    lastAttempt =
                        Remote.init { policy | limit = Policy.MaxAttempts 1 }
                in
                [ ( Retry, attempting ), ( Stop, attempting ), ( RetryAfter 1500, waiting ), ( Retry, lastAttempt ) ]
                    |> List.map
                        (\( decision, remote ) ->
                            Remote.failedAndSchedule decision () "reported failure" remote
                                |> Tuple.first
                                |> Remote.state
                        )
                    |> Expect.equal
                        [ WaitingForRetry { attempts = 1, lastError = "reported failure" }
                        , Failed "reported failure"
                        , WaitingForRetry { attempts = 2, lastError = "reported failure" }
                        , Failed "reported failure"
                        ]
        , test "decisions can stop or extend a wait but cannot exceed the attempt limit" <|
            \_ ->
                let
                    attempting =
                        Remote.init policy

                    ( waiting, firstDelay ) =
                        Remote.failedWith (RetryAfter 1500) "busy" attempting

                    ( waitingAgain, secondDelay ) =
                        waiting |> Remote.retry |> Tuple.first |> Remote.failedWith (RetryAfter 10) "still busy"

                    exhausted =
                        waitingAgain |> Remote.retry |> Tuple.first |> Remote.failedWith (RetryAfter 5000) "unavailable"

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

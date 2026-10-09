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
        [ test "stateToString returns constructor names without payloads" <|
            \_ ->
                let
                    context =
                        { attempts = 2, lastError = "timeout" }
                in
                [ Attempting, WaitingForRetry context, Retrying context, Successful 42, Failed "denied" ]
                    |> List.map Remote.stateToString
                    |> Expect.equal [ "Attempting", "WaitingForRetry", "Retrying", "Successful", "Failed" ]
        , test "success after retries can be lost and recovered without retaining the old value" <|
            \_ ->
                let
                    connecting =
                        Remote.init policy

                    ( waiting, firstDelay ) =
                        Remote.fail "timeout" connecting

                    retrying =
                        Remote.beginRetry waiting |> Tuple.first

                    ( waitingAgain, secondDelay ) =
                        Remote.fail "refused" retrying

                    connected =
                        waitingAgain |> Remote.beginRetry |> Tuple.first |> Remote.succeed "socket"

                    ( lost, resetDelay ) =
                        Remote.fail "closed" connected

                    recovered =
                        Remote.succeed "replacement" lost

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
                                                        Remote.beginRetry >> Tuple.first

                                                    else
                                                        identity
                                                   )
                                                |> Remote.fail (String.fromInt number)
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
                        Remote.fail "timeout" attempting |> Tuple.first

                    retrying =
                        Remote.beginRetry waiting |> Tuple.first

                    successful =
                        Remote.succeed "socket" retrying

                    rejected =
                        Remote.failWith (always Stop) "denied" attempting |> Tuple.first

                    exhausted =
                        retrying
                            |> Remote.fail "refused"
                            |> Tuple.first
                            |> Remote.beginRetry
                            |> Tuple.first
                            |> Remote.fail "unavailable"
                            |> Tuple.first

                    allStates =
                        [ attempting, waiting, retrying, successful, rejected, exhausted ]
                in
                Expect.all
                    [ \_ ->
                        List.map (Remote.beginRetry >> Tuple.mapFirst Remote.state) allStates
                            |> Expect.equal
                                [ ( Remote.state attempting, False )
                                , ( Remote.state retrying, True )
                                , ( Remote.state retrying, False )
                                , ( Remote.state successful, False )
                                , ( Remote.state rejected, False )
                                , ( Remote.state exhausted, False )
                                ]
                    , \_ ->
                        List.map (Remote.fail "new failure" >> Tuple.mapFirst Remote.state) allStates
                            |> Expect.equal
                                [ ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 2, lastError = "new failure" }, Just 2000 )
                                , ( WaitingForRetry { attempts = 2, lastError = "new failure" }, Just 2000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                ]
                    , \_ ->
                        List.map (Remote.failWith (always Stop) "denied again" >> Tuple.mapFirst Remote.state) allStates
                            |> Expect.equal (List.repeat 6 ( Failed "denied again", Nothing ))
                    , \_ ->
                        List.map (Remote.succeed "replacement" >> Remote.state) allStates
                            |> Expect.equal (List.repeat 6 (Successful "replacement"))
                    ]
                    ()
        , test "stored classifiers survive transitions, allow per-call overrides, and can be replaced without resetting state" <|
            \_ ->
                let
                    classify error =
                        if error == "denied" then
                            Stop

                        else
                            RetryAfter 5000

                    initial =
                        Remote.init policy |> Remote.withRetryable classify

                    ( waiting, firstDelay ) =
                        Remote.fail "busy" initial

                    ( waitingAgain, secondDelay ) =
                        waiting |> Remote.beginRetry |> Tuple.first |> Remote.fail "busy"

                    ( exhausted, finalDelay ) =
                        waitingAgain |> Remote.beginRetry |> Tuple.first |> Remote.fail "busy"

                    ( overridden, overrideDelay ) =
                        waiting |> Remote.beginRetry |> Tuple.first |> Remote.failWith (always Retry) "denied"

                    afterSuccess =
                        overridden |> Remote.succeed "value" |> Remote.fail "denied"

                    afterFailure =
                        Remote.fail "busy" exhausted

                    replaced =
                        waiting |> Remote.withRetryable (always Retry)
                in
                Expect.all
                    [ \_ -> Expect.equal [ Just 5000, Just 5000, Nothing ] [ firstDelay, secondDelay, finalDelay ]
                    , \_ -> Expect.equal (Failed "busy") (Remote.state exhausted)
                    , \_ -> Expect.equal ( WaitingForRetry { attempts = 2, lastError = "denied" }, Just 2000 ) ( Remote.state overridden, overrideDelay )
                    , \_ -> Expect.equal ( Failed "denied", Nothing ) (Tuple.mapFirst Remote.state afterSuccess)
                    , \_ -> Expect.equal ( WaitingForRetry { attempts = 1, lastError = "busy" }, Just 5000 ) (Tuple.mapFirst Remote.state afterFailure)
                    , \_ -> Expect.equal (Remote.state waiting) (Remote.state replaced)
                    , \_ ->
                        Remote.fail "denied" replaced
                            |> Tuple.mapFirst Remote.state
                            |> Expect.equal ( WaitingForRetry { attempts = 2, lastError = "denied" }, Just 2000 )
                    ]
                    ()
        , test "the classifier receives each error and its decisions respect the policy" <|
            \_ ->
                let
                    decide error =
                        case error of
                            "busy" ->
                                RetryAfter 1500

                            "still busy" ->
                                RetryAfter 10

                            "unavailable" ->
                                RetryAfter 5000

                            _ ->
                                Stop

                    attempting =
                        Remote.init policy

                    ( waiting, firstDelay ) =
                        Remote.failWith decide "busy" attempting

                    ( waitingAgain, secondDelay ) =
                        waiting |> Remote.beginRetry |> Tuple.first |> Remote.failWith decide "still busy"

                    exhausted =
                        waitingAgain |> Remote.beginRetry |> Tuple.first |> Remote.failWith decide "unavailable"

                    stopped =
                        Remote.failWith decide "denied" attempting
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

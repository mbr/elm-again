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
        [ test "stateToString describes bounded and unlimited retry attempts" <|
            \_ ->
                let
                    labels limit =
                        let
                            initial =
                                Remote.init { policy | limit = limit }

                            waiting =
                                Remote.fail "timeout" initial |> Tuple.first

                            retrying =
                                Remote.beginRetry waiting |> Tuple.first

                            waitingAgain =
                                Remote.fail "timeout" retrying |> Tuple.first

                            retryingAgain =
                                Remote.beginRetry waitingAgain |> Tuple.first

                            successful =
                                Remote.succeed 42 retryingAgain

                            rejected =
                                initial |> Remote.withRetryable (always Stop) |> Remote.fail "denied" |> Tuple.first
                        in
                        [ initial, waiting, retrying, waitingAgain, retryingAgain, successful, rejected ]
                            |> List.map Remote.stateToString
                in
                List.map labels [ Policy.MaxAttempts 3, Policy.Unlimited ]
                    |> Expect.equal
                        [ [ "attempting (attempt 1/3)"
                          , "waiting to retry (attempt 2/3)"
                          , "retrying (attempt 2/3)"
                          , "waiting to retry (attempt 3/3)"
                          , "retrying (attempt 3/3)"
                          , "successful"
                          , "failed"
                          ]
                        , [ "attempting (attempt 1)"
                          , "waiting to retry (attempt 2)"
                          , "retrying (attempt 2)"
                          , "waiting to retry (attempt 3)"
                          , "retrying (attempt 3)"
                          , "successful"
                          , "failed"
                          ]
                        ]
        , test "stateToString includes the initial attempt for limits of one or less" <|
            \_ ->
                [ 1, 0, -1 ]
                    |> List.map (\limit -> Remote.init { policy | limit = Policy.MaxAttempts limit } |> Remote.stateToString)
                    |> Expect.equal (List.repeat 3 "attempting (attempt 1/1)")
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
        , test "failure after success discards the value and respects the stored classifier and limit" <|
            \_ ->
                let
                    outcomes =
                        [ ( 1, Retry, "closed" )
                        , ( 3, Retry, "closed" )
                        , ( 3, RetryAfter 1500, "busy" )
                        , ( 3, Stop, "denied" )
                        ]
                            |> List.map
                                (\( limit, decision, error ) ->
                                    Remote.init { policy | limit = Policy.MaxAttempts limit }
                                        |> Remote.withRetryable (always decision)
                                        |> Remote.succeed "socket"
                                        |> Remote.fail error
                                )
                in
                Expect.all
                    [ \_ ->
                        List.map (Tuple.mapFirst Remote.state) outcomes
                            |> Expect.equal
                                [ ( Failed "closed", Nothing )
                                , ( WaitingForRetry { attempts = 1, lastError = "closed" }, Just 1000 )
                                , ( WaitingForRetry { attempts = 1, lastError = "busy" }, Just 1500 )
                                , ( Failed "denied", Nothing )
                                ]
                    , \_ ->
                        List.map (Tuple.first >> Remote.get) outcomes
                            |> Expect.equal (List.repeat 4 Nothing)
                    , \_ ->
                        List.map (Tuple.first >> Remote.result) outcomes
                            |> Expect.equal [ Just (Err "closed"), Nothing, Nothing, Just (Err "denied") ]
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
                        attempting |> Remote.withRetryable (always Stop) |> Remote.fail "denied" |> Tuple.first

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
                                , ( Failed "new failure", Nothing )
                                , ( WaitingForRetry { attempts = 1, lastError = "new failure" }, Just 1000 )
                                ]
                    , \_ ->
                        List.map (Remote.withRetryable (always Stop) >> Remote.fail "denied again" >> Tuple.mapFirst Remote.state) allStates
                            |> Expect.equal (List.repeat 6 ( Failed "denied again", Nothing ))
                    , \_ ->
                        List.map (Remote.succeed "replacement" >> Remote.state) allStates
                            |> Expect.equal (List.repeat 6 (Successful "replacement"))
                    ]
                    ()
        , test "stored classifiers survive transitions and can be replaced without resetting state" <|
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

                    afterSuccess =
                        waitingAgain |> Remote.succeed "value" |> Remote.fail "denied"

                    afterFailure =
                        Remote.fail "busy" exhausted

                    replaced =
                        waiting |> Remote.withRetryable (always Retry)
                in
                Expect.all
                    [ \_ -> Expect.equal [ Just 5000, Just 5000, Nothing ] [ firstDelay, secondDelay, finalDelay ]
                    , \_ -> Expect.equal (Failed "busy") (Remote.state exhausted)
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
                        Remote.init policy |> Remote.withRetryable decide

                    ( waiting, firstDelay ) =
                        Remote.fail "busy" attempting

                    ( waitingAgain, secondDelay ) =
                        waiting |> Remote.beginRetry |> Tuple.first |> Remote.fail "still busy"

                    exhausted =
                        waitingAgain |> Remote.beginRetry |> Tuple.first |> Remote.fail "unavailable"

                    stopped =
                        Remote.fail "denied" attempting
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

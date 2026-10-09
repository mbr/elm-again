module RemoteCmdTest exposing (tests)

{-| Check the state returned alongside retry commands.
-}

import Again.Decision exposing (Decision(..))
import Again.Policy as Policy
import Again.Remote as Remote exposing (State(..))
import Again.Remote.Cmd as RemoteCmd
import Again.Schedule exposing (Schedule(..))
import Expect
import Test exposing (Test, test)


{-| Check that the command adapter uses the stored classifier and attempt limit.
-}
tests : Test
tests =
    test "RemoteCmd.fail records retries, exhaustion, and failure after success" <|
        \_ ->
            let
                initial =
                    Remote.init { schedule = Periodic { delay = 1000 }, limit = Policy.MaxAttempts 2 }
                        |> Remote.withRetryable
                            (\error ->
                                if error == "denied" then
                                    Stop

                                else
                                    Retry
                            )

                waiting =
                    RemoteCmd.fail () "busy" initial |> Tuple.first

                exhausted =
                    waiting |> Remote.beginRetry |> RemoteCmd.fail () "unavailable" |> Tuple.first

                rejected =
                    initial |> Remote.succeed "connection" |> RemoteCmd.fail () "denied" |> Tuple.first
            in
            [ waiting, exhausted, rejected ]
                |> List.map Remote.state
                |> Expect.equal
                    [ WaitingForRetry { attempts = 1, lastError = "busy" }
                    , Failed "unavailable"
                    , Failed "denied"
                    ]

module Again.Task exposing (retry, retryIf, retryWith)

{-| Run tasks with a retry policy. The first attempt runs immediately; the
schedule controls the waits between subsequent attempts.

Retry a GET up to three total attempts on timeouts or network errors:

    import Again.Policy as Policy
    import Again.Schedule exposing (Schedule(..))
    import Again.Task exposing (retryIf)
    import Http
    import Task

    type Msg
        = ReceivedResponse (Result Http.Error String)

    loadMessage : Cmd Msg
    loadMessage =
        Http.task
            { method = "GET"
            , headers = []
            , url = "/message.txt"
            , body = Http.emptyBody
            , resolver = Http.stringResolver resolveString
            , timeout = Just 5000
            }
            |> retryIf isTransient
                { schedule = Periodic { delay = 1000 }
                , limit = Policy.MaxAttempts 3
                }
            |> Task.attempt ReceivedResponse

    isTransient : Http.Error -> Bool
    isTransient error =
        error == Http.Timeout || error == Http.NetworkError

    resolveString : Http.Response String -> Result Http.Error String
    resolveString response =
        case response of
            Http.GoodStatus_ _ body ->
                Ok body

            Http.BadUrl_ url ->
                Err (Http.BadUrl url)

            Http.Timeout_ ->
                Err Http.Timeout

            Http.NetworkError_ ->
                Err Http.NetworkError

            Http.BadStatus_ metadata _ ->
                Err (Http.BadStatus metadata.statusCode)

@docs retry, retryIf, retryWith

-}

import Again.Decision as Decision exposing (Decision)
import Again.Policy exposing (Policy)
import Process
import Task exposing (Task)


{-| Retries any failure until the task succeeds or the attempt limit is reached.
-}
retry : Policy -> Task error value -> Task error value
retry =
    retryIf (always True)


{-| Retries only errors accepted by the predicate. Returns the last error when
it is rejected or the attempt limit is reached.

For HTTP tasks, [Again.Http.isRetryable](Again-Http#isRetryable) supplies a default predicate.

-}
retryIf : (error -> Bool) -> Policy -> Task error value -> Task error value
retryIf retryable =
    retryWith (retryable >> Decision.fromBool)


{-| Classifies each error with a [Decision](Again-Decision#Decision).
`RetryAfter` waits for the greater of the requested delay and the policy's delay.
Returns the last error when stopped or when the attempt limit is reached.
-}
retryWith : (error -> Decision) -> Policy -> Task error value -> Task error value
retryWith decide policy task =
    attempt decide policy task 1


{-| Runs one attempt, waiting before another if its error and the policy allow it.
-}
attempt : (error -> Decision) -> Policy -> Task error value -> Int -> Task error value
attempt decide policy task count =
    task
        |> Task.onError
            (\error ->
                case Decision.retryDelay (decide error) policy count of
                    Just delay ->
                        Process.sleep delay
                            |> Task.andThen (\_ -> attempt decide policy task (count + 1))

                    Nothing ->
                        Task.fail error
            )

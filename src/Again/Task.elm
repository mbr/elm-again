module Again.Task exposing (retry, retryIf)

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

@docs retry, retryIf

-}

import Again.Policy as Policy exposing (Policy)
import Process
import Task exposing (Task)


{-| Retries any failure until the task succeeds or the attempt limit is reached.
-}
retry : Policy -> Task error value -> Task error value
retry =
    retryIf (always True)


{-| Retries only errors accepted by the predicate. Returns the last error when
it is rejected or the attempt limit is reached.
-}
retryIf : (error -> Bool) -> Policy -> Task error value -> Task error value
retryIf retryable policy task =
    attempt retryable policy task 1


{-| Runs one attempt, waiting before another if its error and the policy allow it.
-}
attempt : (error -> Bool) -> Policy -> Task error value -> Int -> Task error value
attempt retryable policy task count =
    task
        |> Task.onError
            (\error ->
                if retryable error then
                    case Policy.retryDelay policy count of
                        Just delay ->
                            Process.sleep delay
                                |> Task.andThen (\_ -> attempt retryable policy task (count + 1))

                        Nothing ->
                            Task.fail error

                else
                    Task.fail error
            )

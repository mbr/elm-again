# elm-again

Retries for tasks and manually managed values, supporting exponential backoff.

## Retries for tasks

The `Again.Task` module allows you to set the conditions under which a task is retried, the number of attempts, and a backoff schedule such as exponential backoff.

Here is an example that fetches a text file with up to three attempts, retrying transient HTTP failures:

```elm
import Again.Http
import Again.Policy as Policy
import Again.Schedule exposing (Schedule(..))
import Again.Task
import Http
import Task

type Msg
    = ReceivedMessage (Result Http.Error String)

loadMessage : Cmd Msg
loadMessage =
    Http.task
        { method = "GET"
        , headers = []
        , url = "/message.txt"
        , body = Http.emptyBody
        , resolver = Http.stringResolver Again.Http.resolve
        , timeout = Just 5000
        }
        |> Again.Task.retryIf Again.Http.isRetryable
            { schedule = Periodic { delay = 1000 }
            , limit = Policy.MaxAttempts 3
            }
        |> Task.attempt ReceivedMessage
```

## Caller-managed retryable remote data

`Again.Remote` puts you in charge of performing retries but allows you to use the same bookkeeping methods to track success or failure. A successful `Remote` can even fail later, for example when a WebSocket connection closes, and trigger a retry.

As an example, here is how to poll a submitted job. A retry should happen on `HTTP 202 Accepted`, and the job is considered finished on `HTTP 200 OK`. All other responses are considered failures:

```elm
import Again.Decision as Decision
import Again.Policy as Policy
import Again.Remote as Remote
import Again.Remote.Cmd as RemoteCmd
import Again.Schedule exposing (Schedule(..))
import Http

type PollError
    = NotReady String
    | RequestFailed (Http.Response String)

type alias Model =
    Remote.Remote PollError String

type Msg
    = Received (Result PollError String)
    | PollAgain

init : ( Model, Cmd Msg )
init =
    ( Remote.init (Policy.unlimited (Periodic { delay = 1000 }))
        |> Remote.withRetryable decide
    , poll
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Received (Ok output) ->
            ( Remote.succeed output model, Cmd.none )

        Received (Err error) ->
            RemoteCmd.fail PollAgain error model

        PollAgain ->
            ( Remote.beginRetry model, poll )

decide : PollError -> Decision.Decision
decide error =
    case error of
        NotReady _ ->
            Decision.Retry

        RequestFailed _ ->
            Decision.Stop

poll : Cmd Msg
poll =
    Http.get
        { url = "/jobs/42"
        , expect = Http.expectStringResponse Received readStatus
        }

readStatus : Http.Response String -> Result PollError String
readStatus response =
    case response of
        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                200 ->
                    Ok body

                202 ->
                    Err (NotReady body)

                _ ->
                    Err (RequestFailed response)

        _ ->
            Err (RequestFailed response)
```

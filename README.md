# elm-again

Retries for tasks and manually managed values, support exponential backoff.

## `Again.Task`

Fetch `/message.txt` with up to three attempts, retrying transient HTTP failures. `ReceivedMessage` carries the final result to `update`.

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
        , resolver = Http.stringResolver resolveString
        , timeout = Just 5000
        }
        |> Again.Task.retryIf Again.Http.isRetryable
            { schedule = Periodic { delay = 1000 }
            , limit = Policy.MaxAttempts 3
            }
        |> Task.attempt ReceivedMessage

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
```

## `Again.Remote`

A successful `Remote` can later fail, such as a connection that closes.

Poll a submitted job: retry on `202 Accepted`, finish on `200 OK`, and stop on other responses:

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

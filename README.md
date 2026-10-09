# elm-again

Retries for tasks and manually managed values, support exponential backoff.

## `Again.Task`

Retry an existing task up to three times, waiting a second between attempts:

```elm
import Again.Policy as Policy
import Again.Schedule exposing (Schedule(..))
import Again.Task
import Task exposing (Task)

run : Task error value -> Cmd (Result error value)
run task =
    task
        |> Again.Task.retry
            { schedule = Periodic { delay = 1000 }
            , limit = Policy.MaxAttempts 3
            }
        |> Task.attempt identity
```

## `Again.Remote`

A successful `Remote` can later fail, such as a connection that closes. `fail` discards the value and restarts the failure count at one, using the stored classifier and policy. Each attempt can update your model rather than waiting for a task's final result.

The `Again.Remote.Cmd` helpers take care of scheduling retry messages.

Suppose you've submitted a job. Its status endpoint returns `202 Accepted` with progress text while running, `200 OK` with the result when complete, or an error such as `404`. Poll the status endpoint without submitting the job again:

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

`202` is a successful HTTP response, but the job's result isn't ready, so it becomes `NotReady progress`. Each response reaches `update`; the progress text remains in `lastError` while waiting and retrying. The view can inspect `Remote.state model` to display it. On completion, `Remote.get model` returns the result.

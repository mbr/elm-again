module Again.Remote exposing
    ( Remote, State(..), RetryContext
    , init, state, result, get
    , retry, ok, failed, failedWith, failedAndSchedule
    )

{-| Track an operation's value and retries, beginning with its initial attempt.

The caller runs attempts and filters obsolete callbacks. Discard the remote to
abandon tracking; this does not cancel pending effects.

@docs Remote, State, RetryContext
@docs init, state, result, get
@docs retry, ok, failed, failedWith, failedAndSchedule

-}

import Again.Decision as Decision exposing (Decision)
import Again.Policy exposing (Policy)
import Process
import Task


{-| A value with retry state.
-}
type Remote error value
    = Remote Policy (State error value)


{-| The current state of the value.

`Attempting` is the first attempt in flight.

`WaitingForRetry` is waiting before another attempt.

`Retrying` is a subsequent attempt in flight.

`Successful` holds the available value.

`Failed` indicates we have failed and stopped trying.

-}
type State error value
    = Attempting
    | WaitingForRetry (RetryContext error)
    | Retrying (RetryContext error)
    | Successful value
    | Failed error


{-| Information about the current retries.

`attempts` counts completed attempts, starting at one.

`lastError` is the most recent failure.

-}
type alias RetryContext error =
    { attempts : Int
    , lastError : error
    }


{-| Creates a remote value with its initial attempt in flight.
-}
init : Policy -> Remote error value
init policy =
    Remote policy Attempting


{-| Inspects the current state.
-}
state : Remote error value -> State error value
state (Remote _ current) =
    current


{-| Returns the current result.

Pending states return `Nothing`.

-}
result : Remote error value -> Maybe (Result error value)
result remote =
    case state remote of
        Successful value ->
            Just (Ok value)

        Failed error ->
            Just (Err error)

        _ ->
            Nothing


{-| Returns a value only while `Successful`.
-}
get : Remote error value -> Maybe value
get =
    result >> Maybe.andThen Result.toMaybe


{-| Marks a pending retry as running.

Returns `True` when moving from `WaitingForRetry` to `Retrying`.
Other states are unchanged and return `False`.

-}
retry : Remote error value -> ( Remote error value, Bool )
retry ((Remote policy current) as remote) =
    case current of
        WaitingForRetry context ->
            ( Remote policy (Retrying context), True )

        _ ->
            ( remote, False )


{-| Records success.

This discards previous failures and attempt counts.

-}
ok : value -> Remote error value -> Remote error value
ok value (Remote policy _) =
    Remote policy (Successful value)


{-| Records a failure and requests a retry.

Use `failedWith` when some errors should not be retried.

Returns the retry delay in milliseconds, or `Nothing` when exhausted.

-}
failed : error -> Remote error value -> ( Remote error value, Maybe Float )
failed =
    failedWith Decision.Retry


{-| Records a failure using your decision about whether to retry the error.

Advances existing retry counts; otherwise starts at one.

Returns a delay in milliseconds, or `Nothing` when retrying is stopped or exhausted.

-}
failedWith : Decision -> error -> Remote error value -> ( Remote error value, Maybe Float )
failedWith decision error (Remote policy current) =
    case current of
        WaitingForRetry context ->
            recordFailure decision error (context.attempts + 1) policy

        Retrying context ->
            recordFailure decision error (context.attempts + 1) policy

        _ ->
            recordFailure decision error 1 policy


{-| Records a failure and schedules a wakeup message when retrying.

Returns `Cmd.none` when retrying is stopped or exhausted.

-}
failedAndSchedule : Decision -> msg -> error -> Remote error value -> ( Remote error value, Cmd msg )
failedAndSchedule decision wakeup error remote =
    failedWith decision error remote
        |> Tuple.mapSecond (wakeAfter wakeup)


{-| Builds the next state and retry delay.
-}
recordFailure : Decision -> error -> Int -> Policy -> ( Remote error value, Maybe Float )
recordFailure decision error attempts policy =
    let
        delay =
            Decision.retryDelay decision policy attempts

        next =
            case delay of
                Just _ ->
                    WaitingForRetry { attempts = attempts, lastError = error }

                Nothing ->
                    Failed error
    in
    ( Remote policy next, delay )


{-| Schedules a message when a retry delay is present.
-}
wakeAfter : msg -> Maybe Float -> Cmd msg
wakeAfter message delay =
    delay
        |> Maybe.map
            (\milliseconds ->
                Process.sleep milliseconds
                    |> Task.perform (always message)
            )
        |> Maybe.withDefault Cmd.none

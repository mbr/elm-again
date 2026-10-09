module Again.Remote exposing
    ( Remote, State(..), RetryContext
    , init, state, result, get
    , started, ok, failed, failedWith
    )

{-| Track a value that can succeed, fail, and be retried.

The caller runs attempts, schedules retries, and filters obsolete callbacks.

@docs Remote, State, RetryContext
@docs init, state, result, get
@docs started, ok, failed, failedWith

-}

import Again.Decision as Decision exposing (Decision)
import Again.Policy exposing (Policy)


{-| A value with retry state.
-}
type Remote error value
    = Remote Policy (State error value)


{-| The current state of the value.

`NotAttempted` means no attempt has started.

`Attempting` is the first attempt in flight.

`WaitingForRetry` is waiting before another attempt.

`Retrying` is a subsequent attempt in flight.

`Successful` holds the available value.

`Failed` indicates we have failed and stopped trying.

-}
type State error value
    = NotAttempted
    | Attempting
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


{-| Creates a value in `NotAttempted` state.
-}
init : Policy -> Remote error value
init policy =
    Remote policy NotAttempted


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


{-| Records that a new attempt has begun.

Preserves context when waiting or retrying; all other states begin a fresh attempt.

-}
started : Remote error value -> Remote error value
started (Remote policy current) =
    case current of
        WaitingForRetry context ->
            Remote policy (Retrying context)

        Retrying context ->
            Remote policy (Retrying context)

        _ ->
            Remote policy Attempting


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

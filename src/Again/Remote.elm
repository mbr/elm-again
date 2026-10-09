module Again.Remote exposing
    ( Remote, State(..), RetryContext
    , init, state, result, get
    , started, ok, failed, failedWith
    )

{-| Track a value that can succeed, fail, and be retried. The caller runs attempts,
schedules retries, and filters obsolete callbacks.

@docs Remote, State, RetryContext
@docs init, state, result, get
@docs started, ok, failed, failedWith

-}

import Again.Decision as Decision exposing (Decision)
import Again.Policy exposing (Policy)


{-| A retry policy and the current state of a value.
-}
type Remote error value
    = Remote Policy (State error value)


{-| The current state of the value

`NotAttempted` means the process to obtain it has not started.

`Attempting` is the first attempt in flight.

`WaitingForRetry` means we are waiting for time to pass until we are allowed to try again.

`Retrying` is that attempt in flight.

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


{-| Information about the current state of retries

`attempts` counts completed attempts, excluding any in flight. `lastError`
is the most recent failure. The first failure has an attempt count of one.

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


{-| Returns the current success or terminal failure. Pending states return `Nothing`.
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


{-| Marks the first attempt or a pending retry as running. Other states are unchanged.
The caller must check that the retry callback is still current before starting it.
-}
started : Remote error value -> Remote error value
started ((Remote policy current) as remote) =
    case current of
        NotAttempted ->
            Remote policy Attempting

        WaitingForRetry context ->
            Remote policy (Retrying context)

        _ ->
            remote


{-| Accepts a success from any state, discarding previous failures and attempt counts.
The caller must check that the result still belongs to the current attempt or resource.
-}
ok : value -> Remote error value -> Remote error value
ok value (Remote policy _) =
    Remote policy (Successful value)


{-| Records a failure and requests a retry according to the policy.
-}
failed : error -> Remote error value -> ( Remote error value, Maybe Float )
failed =
    failedWith Decision.Retry


{-| Records a failure using the decision and policy. Returns a delay in milliseconds
when waiting to retry, or `Nothing` when no retry is offered.

Acts only on `Attempting`, `Retrying`, or `Successful`. Losing a successful value
drops it and counts as the first failure of a fresh sequence. Other states are
unchanged and return no delay.

-}
failedWith : Decision -> error -> Remote error value -> ( Remote error value, Maybe Float )
failedWith decision error ((Remote policy current) as remote) =
    case current of
        Attempting ->
            recordFailure decision error 1 policy

        Retrying context ->
            recordFailure decision error (context.attempts + 1) policy

        Successful _ ->
            recordFailure decision error 1 policy

        _ ->
            ( remote, Nothing )


{-| Chooses a pending retry or terminal failure from the completed attempt count.
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

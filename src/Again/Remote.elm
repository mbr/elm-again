module Again.Remote exposing
    ( Remote, State(..), RetryContext
    , init, state, stateToString, result, get
    , beginRetry, succeed, fail, failWith
    )

{-| Retry-aware state for a remote value.

@docs Remote, State, RetryContext
@docs init, state, stateToString, result, get
@docs beginRetry, succeed, fail, failWith

-}

import Again.Decision as Decision exposing (Decision)
import Again.Policy exposing (Policy)


{-| A value with retry state.
-}
type Remote error value
    = Remote Policy (State error value)


{-| The current state of the remote value.
-}
type State error value
    = Attempting
    | WaitingForRetry (RetryContext error)
    | Retrying (RetryContext error)
    | Successful value
    | Failed error


{-| The number of completed attempts and the most recent error.
-}
type alias RetryContext error =
    { attempts : Int
    , lastError : error
    }


{-| Creates a remote value in `Attempting` state.
-}
init : Policy -> Remote error value
init policy =
    Remote policy Attempting


{-| Inspects the current state.
-}
state : Remote error value -> State error value
state (Remote _ current) =
    current


{-| Returns the state constructor's name.
-}
stateToString : State error value -> String
stateToString current =
    case current of
        Attempting ->
            "Attempting"

        WaitingForRetry _ ->
            "WaitingForRetry"

        Retrying _ ->
            "Retrying"

        Successful _ ->
            "Successful"

        Failed _ ->
            "Failed"


{-| Returns the success or terminal error, or `Nothing` while pending.
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


{-| Moves `WaitingForRetry` to `Retrying`, returning `True` if changed.
Other states return unchanged with `False`.
-}
beginRetry : Remote error value -> ( Remote error value, Bool )
beginRetry ((Remote policy current) as remote) =
    case current of
        WaitingForRetry context ->
            ( Remote policy (Retrying context), True )

        _ ->
            ( remote, False )


{-| Stores a successful value and clears the retry context.
-}
succeed : value -> Remote error value -> Remote error value
succeed value (Remote policy _) =
    Remote policy (Successful value)


{-| Records a failure with `Decision.Retry`. Returns a delay in milliseconds,
or `Nothing` if the attempt limit is reached.
-}
fail : error -> Remote error value -> ( Remote error value, Maybe Float )
fail =
    failWith (always Decision.Retry)


{-| Classifies a failure and applies the policy. Returns a retry delay in
milliseconds, or `Nothing` if no retry is allowed.
-}
failWith : (error -> Decision) -> error -> Remote error value -> ( Remote error value, Maybe Float )
failWith decide error (Remote policy current) =
    let
        decision =
            decide error
    in
    case current of
        WaitingForRetry context ->
            failureOutcome decision error (context.attempts + 1) policy

        Retrying context ->
            failureOutcome decision error (context.attempts + 1) policy

        _ ->
            failureOutcome decision error 1 policy


{-| Builds the next state and retry delay.
-}
failureOutcome : Decision -> error -> Int -> Policy -> ( Remote error value, Maybe Float )
failureOutcome decision error attempts policy =
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

module Again.Remote exposing
    ( Remote, State(..), RetryContext
    , init, withRetryable, state, stateToString, result, get
    , beginRetry, succeed, fail
    )

{-| Keep track of a remote value and retry it when things go wrong.

A successful value can fail later. For example, a connection may close after it
has opened.

@docs Remote, State, RetryContext
@docs init, withRetryable, state, stateToString, result, get
@docs beginRetry, succeed, fail

-}

import Again.Decision as Decision exposing (Decision)
import Again.Policy as Policy exposing (Policy)


{-| A remote value together with its retry policy and error classifier.
-}
type Remote error value
    = Remote Policy (error -> Decision) (State error value)


{-| See whether your value is available, still being attempted, or has failed.
-}
type State error value
    = Attempting
    | WaitingForRetry (RetryContext error)
    | Retrying (RetryContext error)
    | Successful value
    | Failed error


{-| The number of completed attempts and the latest error in a retry sequence.
-}
type alias RetryContext error =
    { attempts : Int
    , lastError : error
    }


{-| Start tracking a value with its first attempt already under way. By default,
all failures are retryable, subject to your policy.
-}
init : Policy -> Remote error value
init policy =
    Remote policy (always Decision.Retry) Attempting


{-| Choose which errors should be retried. Changing the classifier leaves your
current value and retry progress alone.
-}
withRetryable : (error -> Decision) -> Remote error value -> Remote error value
withRetryable classify (Remote policy _ current) =
    Remote policy classify current


{-| Look at the current state of your remote value.
-}
state : Remote error value -> State error value
state (Remote _ _ current) =
    current


{-| Describe your remote value with a readable label, such as
`"waiting to retry (attempt 2/3)"`. Unlimited policies leave out the total.
-}
stateToString : Remote error value -> String
stateToString (Remote policy _ current) =
    case current of
        Attempting ->
            "attempting" ++ attemptLabel policy 1

        WaitingForRetry context ->
            "waiting to retry" ++ attemptLabel policy (context.attempts + 1)

        Retrying context ->
            "retrying" ++ attemptLabel policy (context.attempts + 1)

        Successful _ ->
            "successful"

        Failed _ ->
            "failed"


{-| Build the attempt label, including a total when the policy has a limit.
-}
attemptLabel : Policy -> Int -> String
attemptLabel policy attempt =
    let
        total =
            Policy.attemptsLeft policy attempt
                |> Maybe.map (\remaining -> "/" ++ String.fromInt (attempt + remaining))
                |> Maybe.withDefault ""
    in
    " (attempt " ++ String.fromInt attempt ++ total ++ ")"


{-| Get the successful value or the error that ended the retry sequence.
You get `Nothing` while an attempt or retry is pending.
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


{-| Get the value if it is currently successful.
-}
get : Remote error value -> Maybe value
get =
    result >> Maybe.andThen Result.toMaybe


{-| Tell the `Remote` value that the wait time has elapsed. It will assume you
are actually retrying afterwards.

This keeps your retry context, if any. Otherwise you start a fresh attempt,
so any previous value or error is discarded.

-}
beginRetry : Remote error value -> Remote error value
beginRetry ((Remote policy classify current) as remote) =
    case current of
        WaitingForRetry context ->
            Remote policy classify (Retrying context)

        Retrying _ ->
            remote

        _ ->
            Remote policy classify Attempting


{-| Tell the `Remote` that an attempt succeeded. This keeps the value and clears
its previous failures and attempt count.
-}
succeed : value -> Remote error value -> Remote error value
succeed value (Remote policy classify _) =
    Remote policy classify (Successful value)


{-| Tell the `Remote` that an attempt failed, or that a successful value was lost.
The stored classifier and policy decide whether to retry.

You get a retry delay in milliseconds, or `Nothing` to give up. A failure after
success discards the value and starts counting failures from one again.

-}
fail : error -> Remote error value -> ( Remote error value, Maybe Float )
fail error (Remote policy classify current) =
    let
        attempts =
            case current of
                WaitingForRetry context ->
                    context.attempts + 1

                Retrying context ->
                    context.attempts + 1

                _ ->
                    1
    in
    failureOutcome (classify error) error attempts policy
        |> Tuple.mapFirst (Remote policy classify)


{-| Work out whether to wait for another attempt or keep the final error.
-}
failureOutcome : Decision -> error -> Int -> Policy -> ( State error value, Maybe Float )
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
    ( next, delay )

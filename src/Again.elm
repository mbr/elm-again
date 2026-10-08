module Again exposing
    ( Model, Policy, AttemptId
    , Event(..), Action(..), Status(..)
    , init, update, status, attempts, value, lastError
    , constant, exponential, withJitter, withMaxAttempts, withResetAfter
    )

{-| A pure retry controller. Callers supply time, execute attempts, and schedule
wakeups. This module performs no effects and is independent of the application.

@docs Model, Policy, AttemptId
@docs Event, Action, Status
@docs init, update, status, attempts, value, lastError
@docs constant, exponential, withJitter, withMaxAttempts, withResetAfter

-}

import Random
import Time exposing (Posix)


{-| Retry configuration. Durations are milliseconds; retries are unlimited unless
limited explicitly. Invalid numeric settings are normalized to finite bounds.
-}
type Policy
    = Policy Settings


{-| Internal normalized policy settings.
-}
type alias Settings =
    { initialDelay : Float
    , multiplier : Float
    , maxDelay : Float
    , jitter : Float
    , maxAttempts : Maybe Int
    , resetAfter : Float
    }


{-| Identifies an execution, independently of the resettable attempt count.
-}
type AttemptId
    = AttemptId Int


{-| Lifecycle input. `Start` starts an idle or exhausted controller. `Invalidated`
retries a previously successful value without discarding it. Results for old
attempts and events inappropriate for the current phase are ignored.
-}
type Event error value
    = Start
    | Succeeded AttemptId value
    | Failed AttemptId error
    | Invalidated error
    | TimerElapsed


{-| Effects to interpret outside the controller. Return execution results with
exactly the supplied attempt ID. Feed `TimerElapsed` when a wakeup is due.
-}
type Action
    = Run AttemptId
    | WakeAt Posix


{-| Observable lifecycle, separate from the cached value and failure.
-}
type Status
    = Idle
    | Running
    | Waiting Posix
    | Ready
    | Exhausted


{-| Internal lifecycle with execution identity and success time.
-}
type Phase
    = Initial
    | InFlight AttemptId
    | Successful Posix
    | Sleeping Posix
    | Stopped


{-| Opaque retry state, retaining the last successful value across failures.
-}
type Model error value
    = Model
        { policy : Settings
        , phase : Phase
        , count : Int
        , serial : Int
        , seed : Random.Seed
        , cached : Maybe value
        , error : Maybe error
        }


{-| Uses a fixed delay between attempts.
-}
constant : Float -> Policy
constant delay =
    exponential { initialDelay = delay, multiplier = 1, maxDelay = delay }


{-| Grows delays exponentially, capped at the configured maximum.
-}
exponential : { initialDelay : Float, multiplier : Float, maxDelay : Float } -> Policy
exponential config =
    Policy
        { initialDelay = nonnegative config.initialDelay
        , multiplier = max 1 (finite config.multiplier)
        , maxDelay = nonnegative config.maxDelay
        , jitter = 0
        , maxAttempts = Nothing
        , resetAfter = 0
        }


{-| Varies delays by a fraction between zero and one, still bounded by the maximum.
The supplied seed makes the sequence deterministic; distinct clients should use
distinct seeds.
-}
withJitter : Float -> Policy -> Policy
withJitter fraction (Policy policy) =
    Policy { policy | jitter = clamp 0 1 (finite fraction) }


{-| Limits attempts, including the initial execution, to at least one.
-}
withMaxAttempts : Int -> Policy -> Policy
withMaxAttempts limit (Policy policy) =
    Policy { policy | maxAttempts = Just (max 1 limit) }


{-| Resets the attempt count when a successful value is invalidated after this
stable duration. Earlier invalidation continues the existing backoff sequence.
The default duration is zero.
-}
withResetAfter : Float -> Policy -> Policy
withResetAfter duration (Policy policy) =
    Policy { policy | resetAfter = nonnegative duration }


{-| Creates an idle controller without issuing an attempt.
-}
init : Policy -> Random.Seed -> Model error value
init (Policy policy) seed =
    Model
        { policy = policy
        , phase = Initial
        , count = 0
        , serial = 0
        , seed = seed
        , cached = Nothing
        , error = Nothing
        }


{-| Applies input using caller-supplied time. Supply nondecreasing times. Duplicate
wakeups are harmless, and early wakeups never start an attempt prematurely.
-}
update : Posix -> Event error value -> Model error value -> ( Model error value, List Action )
update now event ((Model model) as original) =
    case ( event, model.phase ) of
        ( Start, Initial ) ->
            run original

        ( Start, Stopped ) ->
            run (Model { model | count = 0, error = Nothing })

        ( Succeeded id result, InFlight current ) ->
            if id == current then
                ( Model { model | phase = Successful now, cached = Just result, error = Nothing }, [] )

            else
                ( original, [] )

        ( Failed id error, InFlight current ) ->
            if id == current then
                wait now error original

            else
                ( original, [] )

        ( Invalidated error, Successful since ) ->
            let
                stable =
                    toFloat (Time.posixToMillis now - Time.posixToMillis since) >= model.policy.resetAfter
            in
            wait now
                error
                (Model
                    { model
                        | count =
                            if stable then
                                0

                            else
                                model.count
                    }
                )

        ( TimerElapsed, Sleeping deadline ) ->
            if Time.posixToMillis now >= Time.posixToMillis deadline then
                run original

            else
                ( original, [] )

        _ ->
            ( original, [] )


{-| Begins one execution with a fresh identity.
-}
run : Model error value -> ( Model error value, List Action )
run (Model model) =
    let
        id =
            AttemptId (model.serial + 1)
    in
    ( Model { model | phase = InFlight id, serial = model.serial + 1, count = model.count + 1 }
    , [ Run id ]
    )


{-| Schedules a retry or records exhaustion without discarding cached data.
-}
wait : Posix -> error -> Model error value -> ( Model error value, List Action )
wait now error (Model model) =
    if Maybe.map (\limit -> model.count >= limit) model.policy.maxAttempts |> Maybe.withDefault False then
        ( Model { model | phase = Stopped, error = Just error }, [] )

    else
        let
            base =
                if model.policy.initialDelay == 0 then
                    0

                else
                    min model.policy.maxDelay
                        (model.policy.initialDelay * model.policy.multiplier ^ toFloat (max 0 (model.count - 1)))

            ( factor, seed ) =
                Random.step (Random.float (1 - model.policy.jitter) (1 + model.policy.jitter)) model.seed

            delay =
                min model.policy.maxDelay (base * factor)

            deadline =
                Time.millisToPosix (Time.posixToMillis now + ceiling delay)
        in
        ( Model { model | phase = Sleeping deadline, seed = seed, error = Just error }
        , [ WakeAt deadline ]
        )


{-| Returns the observable lifecycle.
-}
status : Model error value -> Status
status (Model model) =
    case model.phase of
        Initial ->
            Idle

        InFlight _ ->
            Running

        Successful _ ->
            Ready

        Sleeping deadline ->
            Waiting deadline

        Stopped ->
            Exhausted


{-| Counts executions in the current backoff sequence.
-}
attempts : Model error value -> Int
attempts (Model model) =
    model.count


{-| Returns the last successful value, including while retrying or exhausted.
-}
value : Model error value -> Maybe value
value (Model model) =
    model.cached


{-| Returns the latest failure, cleared by success or an explicit restart.
-}
lastError : Model error value -> Maybe error
lastError (Model model) =
    model.error


{-| Normalizes duration settings.
-}
nonnegative : Float -> Float
nonnegative number =
    max 0 (finite number)


{-| Excludes non-finite policy settings.
-}
finite : Float -> Float
finite number =
    if isNaN number || isInfinite number then
        0

    else
        number

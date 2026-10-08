module Again.Policy exposing (Policy(..), delay)

{-| Calculate retry delays without keeping state or consulting a clock.

@docs Policy, delay

-}


{-| Retry delays in milliseconds. `Periodic` uses a fixed delay;
`ExponentialBackoff` grows from `initialDelay` by `multiplier`, capped at
`maxDelay`. `ImmediatelyThen` prepends a zero delay to another policy.

Supply finite, nonnegative delays, a finite multiplier of at least one, and a
maximum no smaller than the initial delay. Parameters are not validated.

-}
type Policy
    = Periodic { delay : Float }
    | ExponentialBackoff
        { initialDelay : Float
        , multiplier : Float
        , maxDelay : Float
        }
    | ImmediatelyThen Policy


{-| Returns a delay for a nonnegative, zero-based retry index. Partially apply a
policy to obtain an `Int -> Float` delay function.
-}
delay : Policy -> Int -> Float
delay policy retryIndex =
    case policy of
        Periodic config ->
            config.delay

        ExponentialBackoff config ->
            if config.initialDelay == 0 then
                0

            else
                min config.maxDelay
                    (config.initialDelay * config.multiplier ^ toFloat retryIndex)

        ImmediatelyThen inner ->
            if retryIndex == 0 then
                0

            else
                delay inner (retryIndex - 1)

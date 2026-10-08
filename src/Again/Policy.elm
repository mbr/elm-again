module Again.Policy exposing (Policy(..), immediately, delay)

{-| Calculate retry delays without keeping state or consulting a clock.

@docs Policy, immediately, delay

-}


{-| Choose how long to wait before each retry. All delays are in milliseconds.

`Periodic` uses the same `delay` for every retry.

`ExponentialBackoff` starts with `initialDelay`, then multiplies the delay by
`multiplier` for each subsequent retry, up to `maxDelay`.

`ImmediatelyThen` retries once without waiting, then follows the wrapped policy
from its first delay. Wrap it around either of the other variants, or nest it
for several immediate retries.

Choose a `multiplier` of one or more and delays of zero or more, with
`maxDelay` at least as large as `initialDelay`. The policy uses your values
as given, so make sure they're finite numbers.

-}
type Policy
    = Periodic { delay : Float }
    | ExponentialBackoff
        { initialDelay : Float
        , multiplier : Float
        , maxDelay : Float
        }
    | ImmediatelyThen Policy


{-| Retry without waiting, every time. Equivalent to `Periodic { delay = 0 }`.
-}
immediately : Policy
immediately =
    Periodic { delay = 0 }


{-| Returns a delay for a nonnegative, zero-based retry index.
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

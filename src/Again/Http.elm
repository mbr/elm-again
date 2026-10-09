module Again.Http exposing (isRetryable)

{-| Classify HTTP failures for retry policies.

@docs isRetryable

-}

import Http


{-| Accepts timeouts, network errors, and status codes 408, 429, 500, 502, 503,
and 504. Other errors are rejected.

Assumes the operation is safe to repeat: a timeout does not mean a write failed.
Does not account for `Retry-After`, since `Http.Error` does not retain headers.

-}
isRetryable : Http.Error -> Bool
isRetryable error =
    case error of
        Http.Timeout ->
            True

        Http.NetworkError ->
            True

        Http.BadStatus code ->
            List.member code [ 408, 429, 500, 502, 503, 504 ]

        Http.BadUrl _ ->
            False

        Http.BadBody _ ->
            False

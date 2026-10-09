module Again.Http exposing (isRetryable, resolve)

{-| Helpers for HTTP tasks and retry decisions.

@docs isRetryable, resolve

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


{-| Keep the body of a successful response, or turn a failed response into an
`Http.Error`. Use with `Http.stringResolver` or `Http.bytesResolver`.
-}
resolve : Http.Response body -> Result Http.Error body
resolve response =
    case response of
        Http.GoodStatus_ _ body ->
            Ok body

        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

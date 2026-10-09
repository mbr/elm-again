module HttpTest exposing (tests)

{-| Checks the default HTTP retry classification.
-}

import Again.Http
import Expect
import Http
import Test exposing (Test, describe, test)


{-| Covers transport failures and the HTTP status allowlist.
-}
tests : Test
tests =
    describe "HTTP retry classification"
        [ test "accepts transport failures but rejects malformed URLs and bodies" <|
            \_ ->
                [ Http.Timeout, Http.NetworkError, Http.BadUrl "invalid", Http.BadBody "invalid" ]
                    |> List.map Again.Http.isRetryable
                    |> Expect.equal [ True, True, False, False ]
        , test "retries only the selected HTTP status codes" <|
            \_ ->
                List.range 100 599
                    |> List.filter (Http.BadStatus >> Again.Http.isRetryable)
                    |> Expect.equal [ 408, 429, 500, 502, 503, 504 ]
        ]

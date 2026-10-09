module HttpTest exposing (tests)

{-| Check HTTP response conversion and retry classification.
-}

import Again.Http
import Dict
import Expect
import Http
import Test exposing (Test, describe, test)


{-| Cover response bodies, transport failures, and HTTP status handling.
-}
tests : Test
tests =
    describe "Again.Http"
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
        , test "resolve preserves successful bodies and converts each HTTP failure" <|
            \_ ->
                [ Http.GoodStatus_ (metadata 200) "hello"
                , Http.BadUrl_ "invalid"
                , Http.Timeout_
                , Http.NetworkError_
                , Http.BadStatus_ (metadata 503) "unavailable"
                ]
                    |> List.map Again.Http.resolve
                    |> Expect.equal
                        [ Ok "hello"
                        , Err (Http.BadUrl "invalid")
                        , Err Http.Timeout
                        , Err Http.NetworkError
                        , Err (Http.BadStatus 503)
                        ]
        ]


{-| Build response metadata for a status code.
-}
metadata : Int -> Http.Metadata
metadata statusCode =
    { url = "/message.txt"
    , statusCode = statusCode
    , statusText = ""
    , headers = Dict.empty
    }

module Commands exposing (..)

import Models exposing (..)
import Msgs exposing (Msg)
import Routing exposing (endpointUrl)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Json.Encode as Encode
import RemoteData
import Time exposing (Time, second)


requestTimeout : Time
requestTimeout =
    5 * second


requestAuth : Email -> Password -> Http.Request AuthenticationResponse
requestAuth email password =
    Http.request
        { body = authRequestEncoder email password |> Http.jsonBody
        , expect = Http.expectJson authResponseDecoder
        , headers = []
        , method = "POST"
        , timeout = Just requestTimeout
        , url = endpointUrl Auth
        , withCredentials = False
        }


requestAuthCmd : Email -> Password -> Cmd Msg
requestAuthCmd email password =
    requestAuth email password
        |> Http.send Msgs.OnAuthenticationResponse


authRequestEncoder : Email -> Password -> Encode.Value
authRequestEncoder email password =
    let
        attributes =
            [ ( "email", Encode.string email )
            , ( "password", Encode.string password )
            ]
    in
        Encode.object attributes


authResponseErrorConstructor : String -> String -> AuthenticationResponse
authResponseErrorConstructor code message =
    let
        error =
            case code of
                "BadEmailPassword" ->
                    BadEmailPasswordError

                _ ->
                    UnknownAuthenticationError code
    in
        AuthenticationErrorResponse
            { error = error
            , message = message
            }


authResponseErrorDecoder : Decode.Decoder AuthenticationResponse
authResponseErrorDecoder =
    DecodePipeline.decode authResponseErrorConstructor
        |> DecodePipeline.required "code" Decode.string
        |> DecodePipeline.required "message" Decode.string
        |> Decode.field "error"


authResponseSuccessDecoder : Decode.Decoder AuthenticationResponse
authResponseSuccessDecoder =
    DecodePipeline.decode
        (\token tokenId validFor ->
            AuthenticationSuccessResponse
                { token = token
                , tokenId = tokenId
                , validFor = validFor
                }
        )
        |> DecodePipeline.required "token" Decode.string
        |> DecodePipeline.required "tokenId" Decode.string
        |> DecodePipeline.required "validFor" Decode.int


authResponseDecoder : Decode.Decoder AuthenticationResponse
authResponseDecoder =
    Decode.oneOf [ authResponseErrorDecoder, authResponseSuccessDecoder ]

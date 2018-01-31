port module Helpers.Authentication exposing (..)

import Globals.Types
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Json.Encode as Encode
import List
import RemoteData
import Task
import Time exposing (second)


port saveAuthLocalStorage : Encode.Value -> Cmd msg


encodeAuthLocalStorage : Globals.Types.Authentication -> Encode.Value
encodeAuthLocalStorage auth =
    Encode.object
        [ ( "token", Encode.string auth.token )
        , ( "tokenId", Encode.string auth.tokenId )
        , ( "validUntil", Encode.float auth.validUntil )
        ]


type alias AuthenticationSuccessResponseContent =
    { token : String
    , tokenId : String
    , validFor : Int
    }


type AuthenticationResponse
    = AuthenticationSuccessResponse AuthenticationSuccessResponseContent
    | AuthenticationErrorResponse
        { error : AuthenticationError
        , message : String
        }


type AuthenticationError
    = BadEmailPasswordError
    | UnknownAuthenticationError String


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


requestTimeout =
    5 * second


authRequest : String -> String -> String -> Http.Request AuthenticationResponse
authRequest baseUrl email password =
    Http.request
        { body = authRequestEncode email password |> Http.jsonBody
        , expect = expectJsonLog authResponseDecode
        , headers = []
        , method = "POST"
        , timeout = Just requestTimeout
        , url = baseUrl ++ "/auth"
        , withCredentials = False
        }


expectJsonLog : Decode.Decoder a -> Http.Expect a
expectJsonLog decoder =
    Http.expectStringResponse <|
        \response ->
            let
                _ =
                    Debug.log "DECODE!!!!!!!!" "blubb"
            in
            case Decode.decodeString decoder response.body of
                Err decodeError ->
                    Err "DecodeError"

                -- (Decode.errorToString decodeError)
                Ok value ->
                    Ok value


authRequestEncode : String -> String -> Encode.Value
authRequestEncode email password =
    let
        attributes =
            [ ( "email", Encode.string email )
            , ( "password", Encode.string password )
            ]
    in
    Encode.object attributes


authResponseErrorDecoder : Decode.Decoder AuthenticationResponse
authResponseErrorDecoder =
    DecodePipeline.decode
        (\code message ->
            let
                error =
                    case code of
                        "BadEmailPassword" ->
                            BadEmailPasswordError

                        _ ->
                            UnknownAuthenticationError code
            in
            AuthenticationErrorResponse { error = error, message = message }
        )
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


authResponseDecode : Decode.Decoder AuthenticationResponse
authResponseDecode =
    Decode.oneOf [ authResponseErrorDecoder, authResponseSuccessDecoder ]


saveAuthentication : AuthenticationSuccessResponseContent -> Time.Time -> Cmd Globals.Types.Msg
saveAuthentication authRes time =
    let
        auth =
            { token = authRes.token
            , tokenId = authRes.tokenId
            , validUntil = time + toFloat authRes.validFor * Time.second
            }
    in
    Cmd.batch
        [ send <| Globals.Types.SaveAuthentication auth
        , saveAuthLocalStorage <| encodeAuthLocalStorage auth
        ]


authenticationHeaders : Globals.Types.Authentication -> List Http.Header
authenticationHeaders auth =
    List.singleton <| Http.header "Authorization" <| "Bearer " ++ auth.token


isAuthenticated : Globals.Types.Model -> Bool
isAuthenticated globals =
    case globals.auth of
        Just auth ->
            case globals.time of
                Just time ->
                    auth.validUntil >= time

                Nothing ->
                    -- Let's hope the token is still valid, as soon as the
                    -- first time tick arrives we'll get thrown out otherwise
                    True

        Nothing ->
            False

port module Helpers.Authentication exposing (..)

import Globals.Types exposing (Authentication)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Json.Encode as Encode
import List
import Task
import Time exposing (second)


port saveAuthLocalStorage : Encode.Value -> Cmd msg


encodeAuthLocalStorage : Authentication -> Encode.Value
encodeAuthLocalStorage auth =
    Encode.object
        [ ( "serverUrl", Encode.string auth.serverUrl )
        , ( "token", Encode.string auth.token )
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


type SignupResponse
    = SignupSuccessResponse { email : String, emailVerified : Bool }
      -- TODO: Handle errors
    | SignupErrorResponse


type AuthenticationError
    = BadEmailPasswordError
    | UnknownAuthenticationError String


type alias SignupRequestData =
    { firstName : String
    , lastName : String
    , email : String
    , password : String
    , invitationCode : Maybe String
    }


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


requestTimeout : Float
requestTimeout =
    5 * second


authRequest : String -> String -> String -> Http.Request AuthenticationResponse
authRequest serverUrl email password =
    Http.request
        { body = authRequestEncode email password |> Http.jsonBody
        , expect = Http.expectJson authResponseDecode
        , headers = []
        , method = "POST"
        , timeout = Just requestTimeout
        , url = serverUrl ++ "/auth"
        , withCredentials = False
        }


signupRequest : String -> SignupRequestData -> Http.Request SignupResponse
signupRequest serverUrl data =
    Http.request
        { body = signupRequestEncode data |> Http.jsonBody
        , expect = Http.expectJson signupResponseSuccessDecoder
        , headers = []
        , method = "POST"
        , timeout = Just requestTimeout
        , url = serverUrl ++ "/users"
        , withCredentials = False
        }


authRequestEncode : String -> String -> Encode.Value
authRequestEncode email password =
    let
        attributes =
            [ ( "email", Encode.string email )
            , ( "password", Encode.string password )
            ]
    in
    Encode.object attributes


signupRequestEncode : SignupRequestData -> Encode.Value
signupRequestEncode data =
    Encode.object <|
        [ ( "firstName", Encode.string data.firstName )
        , ( "lastName", Encode.string data.lastName )
        , ( "password", Encode.string data.password )
        ]
            ++ (case data.invitationCode of
                    Nothing ->
                        [ ( "email", Encode.string data.email ) ]

                    Just code ->
                        [ ( "invitationCode", Encode.string code ) ]
               )


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


decodeSignupResponse : Result Http.Error SignupResponse -> SignupResponse
decodeSignupResponse res =
    case res of
        Ok success ->
            success

        Err _ ->
            SignupErrorResponse


signupResponseSuccessDecoder : Decode.Decoder SignupResponse
signupResponseSuccessDecoder =
    DecodePipeline.decode
        (\email emailVerified ->
            SignupSuccessResponse
                { email = email
                , emailVerified = emailVerified
                }
        )
        |> DecodePipeline.required "email" Decode.string
        |> DecodePipeline.required "emailVerified" Decode.bool


toAuthentication : String -> AuthenticationSuccessResponseContent -> Time.Time -> Authentication
toAuthentication serverUrl authRes time =
    { serverUrl = serverUrl
    , token = authRes.token
    , tokenId = authRes.tokenId
    , validUntil = time + toFloat authRes.validFor * Time.second
    }


saveAuthentication : Authentication -> Cmd Globals.Types.Msg
saveAuthentication auth =
    Cmd.batch
        [ send <| Globals.Types.SaveAuthentication auth
        , saveAuthLocalStorage <| encodeAuthLocalStorage auth
        ]


authenticationHeaders : Authentication -> List Http.Header
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


getValidAuth : Globals.Types.Model -> Maybe Authentication
getValidAuth globals =
    case globals.auth of
        Just auth ->
            case globals.time of
                Just time ->
                    if auth.validUntil >= time then
                        Just auth
                    else
                        Nothing

                Nothing ->
                    Just auth

        Nothing ->
            Nothing

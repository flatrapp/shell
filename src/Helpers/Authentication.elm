port module Helpers.Authentication exposing (..)

import Globals.Types exposing (Authentication)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Json.Encode as Encode
import List
import Task
import Time exposing (second)


requestTimeout : Float
requestTimeout =
    5 * second


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity



-- ////////// AUTHENTICATION //////////


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


port saveAuthLocalStorage : Encode.Value -> Cmd msg


encodeAuthLocalStorage : Authentication -> Encode.Value
encodeAuthLocalStorage auth =
    Encode.object
        [ ( "serverUrl", Encode.string auth.serverUrl )
        , ( "token", Encode.string auth.token )
        , ( "tokenId", Encode.string auth.tokenId )
        , ( "validUntil", Encode.float auth.validUntil )
        ]



-- ////////// LOGIN //////////


type AuthenticationResponse
    = AuthenticationSuccessResponse AuthenticationSuccessResponseContent
    | AuthenticationErrorResponse
        { error : AuthenticationError
        , message : String
        }
    | AuthenticationInvalidResponse
    | AuthenticationHttpError Http.Error


type alias AuthenticationSuccessResponseContent =
    { token : String
    , tokenId : String
    , validFor : Int
    }


type AuthenticationError
    = BadCredentialsError
    | EmailNotVerifiedError
    | UnknownAuthenticationError String


authRequest : String -> String -> String -> Http.Request AuthenticationResponse
authRequest serverUrl email password =
    Http.request
        { body = authRequestEncode email password |> Http.jsonBody
        , expect = Http.expectJson authResponseSuccessDecoder
        , headers = []
        , method = "POST"
        , timeout = Just requestTimeout
        , url = serverUrl ++ "/auth"
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


authResponseDecode : Result Http.Error AuthenticationResponse -> AuthenticationResponse
authResponseDecode res =
    case res of
        Ok success ->
            success

        Err (Http.BadStatus errRes) ->
            case Decode.decodeString authResponseErrorDecoder errRes.body of
                Err _ ->
                    AuthenticationInvalidResponse

                Ok authRes ->
                    authRes

        Err httpError ->
            AuthenticationHttpError httpError


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


authResponseErrorDecoder : Decode.Decoder AuthenticationResponse
authResponseErrorDecoder =
    DecodePipeline.decode
        (\code message ->
            let
                error =
                    case code of
                        "credentials_wrong" ->
                            BadCredentialsError

                        "email_not_verified" ->
                            EmailNotVerifiedError

                        _ ->
                            UnknownAuthenticationError code
            in
            AuthenticationErrorResponse { error = error, message = message }
        )
        |> DecodePipeline.required "code" Decode.string
        |> DecodePipeline.required "message" Decode.string
        |> Decode.field "error"



-- ////////// SIGNUP //////////


type alias SignupRequestData =
    { firstName : String
    , lastName : String
    , email : String
    , password : String
    , invitationCode : Maybe String
    }


type SignupResponse
    = SignupSuccessResponse { email : String, emailVerified : Bool }
    | SignupErrorResponse { error : SignupError, message : String }
    | SignupInvalidResponse
    | SignupHttpError Http.Error


type SignupError
    = EmailCollisionError
    | NotInvitedError
    | InvitationCodeInvalidError
    | UnknownSignupError String


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
                        [ ( "invitation_code", Encode.string code ) ]
               )


signupResponseDecode : Result Http.Error SignupResponse -> SignupResponse
signupResponseDecode res =
    case res of
        Ok success ->
            success

        Err (Http.BadStatus errRes) ->
            case Decode.decodeString signupResponseErrorDecoder errRes.body of
                Err _ ->
                    SignupInvalidResponse

                Ok signupRes ->
                    signupRes

        Err httpError ->
            SignupHttpError httpError


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


signupResponseErrorDecoder : Decode.Decoder SignupResponse
signupResponseErrorDecoder =
    DecodePipeline.decode
        (\code message ->
            let
                error =
                    case code of
                        "email_exists" ->
                            EmailCollisionError

                        "not_invited" ->
                            NotInvitedError

                        "invitation_code_invalid" ->
                            InvitationCodeInvalidError

                        _ ->
                            UnknownSignupError code
            in
            SignupErrorResponse { error = error, message = message }
        )
        |> DecodePipeline.required "code" Decode.string
        |> DecodePipeline.required "message" Decode.string
        |> Decode.field "error"

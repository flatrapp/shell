module Helpers.Api.Authentication exposing (..)

import Helpers.Authentication exposing (..)
import Http
import Json.Decode as Decode
import Helpers.Functions exposing (..)
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Globals.Types exposing (Authentication)
import Time exposing (Time, second)

requestTimeout : Float
requestTimeout =
    5 * second


toAuthentication : AuthenticationSuccessResponseContent -> Time -> Authentication
toAuthentication authRes time =
    { serverUrl = authRes.serverUrl
    , token = authRes.token
    , tokenId = authRes.tokenId
    , validUntil = time + toFloat authRes.validFor * Time.second
    }

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
    , serverUrl : String
    }


type AuthenticationError
    = BadCredentialsError
    | EmailNotVerifiedError
    | UnknownAuthenticationError String


authRequest : String -> String -> String -> Http.Request AuthenticationResponse
authRequest serverUrl email password =
    Http.request
        { body = authRequestEncode email password |> Http.jsonBody
        , expect = Http.expectJson <| authResponseSuccessDecoder serverUrl
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
    responseDecode
        authResponseErrorDecoder
        AuthenticationInvalidResponse
        AuthenticationHttpError
        res


authResponseSuccessDecoder : String -> Decode.Decoder AuthenticationResponse
authResponseSuccessDecoder serverUrl =
    DecodePipeline.decode
        (\token tokenId validFor ->
            AuthenticationSuccessResponse
                { token = token
                , tokenId = tokenId
                , validFor = validFor
                , serverUrl = serverUrl
                }
        )
        |> DecodePipeline.required "token" Decode.string
        |> DecodePipeline.required "tokenId" Decode.string
        |> DecodePipeline.required "validFor" Decode.int


authResponseErrorDecoder : Decode.Decoder AuthenticationResponse
authResponseErrorDecoder =
    errorDecoder
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
        , ( "absent", Encode.bool False )
        ]
            ++ (case data.invitationCode of
                    Nothing ->
                        [ ( "email", Encode.string data.email ) ]

                    Just code ->
                        [ ( "invitation_code", Encode.string code ) ]
               )


signupResponseDecode : Result Http.Error SignupResponse -> SignupResponse
signupResponseDecode res =
    responseDecode
        signupResponseErrorDecoder
        SignupInvalidResponse
        SignupHttpError
        res


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
    errorDecoder
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

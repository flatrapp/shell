module Helpers.User exposing (..)

import Globals.Types exposing (Authentication)
import Helpers.Authentication exposing (authenticationHeaders)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Time


requestTimeout : Float
requestTimeout =
    5 * Time.second



-- ////////// ////////// //////////


type alias UserInfo =
    { id : Int
    , email : String
    , firstName : String
    , lastName : String
    }


type CurrentUserResponse
    = CurrentUserSuccessResponse UserInfo
    | CurrentUserErrorResponse { error : CurrentUserError, message : String }
    | CurrentUserInvalidResponse
    | CurrentUserHttpError Http.Error


type CurrentUserError
    = CurrentUserUnauthorizedError
    | CurrentUserUnknownError String


currentUserRequest : Authentication -> Http.Request CurrentUserResponse
currentUserRequest auth =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson currentUserSuccessDecoder
        , headers = authenticationHeaders auth
        , method = "GET"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/users/current"
        , withCredentials = False
        }


currentUserSuccessDecoder : Decode.Decoder CurrentUserResponse
currentUserSuccessDecoder =
    DecodePipeline.decode
        (\id email firstName lastName ->
            CurrentUserSuccessResponse
                { id = id
                , email = email
                , firstName = firstName
                , lastName = lastName
                }
        )
        |> DecodePipeline.required "id" Decode.int
        |> DecodePipeline.required "email" Decode.string
        |> DecodePipeline.required "firstName" Decode.string
        |> DecodePipeline.required "lastName" Decode.string


currentUserErrorDecoder : Decode.Decoder CurrentUserResponse
currentUserErrorDecoder =
    DecodePipeline.decode
        (\code message ->
            CurrentUserErrorResponse
                { error =
                    case code of
                        "unauthorized" ->
                            CurrentUserUnauthorizedError

                        errorCode ->
                            CurrentUserUnknownError errorCode
                , message = message
                }
        )
        |> DecodePipeline.required "code" Decode.string
        |> DecodePipeline.required "message" Decode.string
        |> Decode.field "error"


currentUserResponseDecode : Result Http.Error CurrentUserResponse -> CurrentUserResponse
currentUserResponseDecode res =
    case res of
        Ok success ->
            success

        Err (Http.BadStatus response) ->
            case Decode.decodeString currentUserErrorDecoder response.body of
                Err decodeError ->
                    CurrentUserInvalidResponse

                Ok parsedResponse ->
                    parsedResponse

        Err (Http.BadPayload _ _) ->
            CurrentUserInvalidResponse

        Err httpErr ->
            CurrentUserHttpError httpErr

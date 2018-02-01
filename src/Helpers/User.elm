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


type alias UserInfo =
    { id : Int
    , email : String
    , firstName : String
    , lastName : String
    }


type CurrentUserAPIError
    = CurrentUserUnknownError


type CurrentUserResponse
    = CurrentUserSuccessResponse UserInfo
    | CurrentUserErrorResponse { error : CurrentUserAPIError, message : String }
    | CurrentUserHttpError Http.Error
    | CurrentUserParsingError


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


currentUserErrorDecoder : Decode.Decoder CurrentUserResponse
currentUserErrorDecoder =
    DecodePipeline.decode
        (\code message ->
            CurrentUserErrorResponse { error = CurrentUserUnknownError, message = message }
        )
        |> DecodePipeline.required "code" Decode.string
        |> DecodePipeline.required "message" Decode.string
        |> Decode.field "error"


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


decodeCurrentUserResponse : Result Http.Error CurrentUserResponse -> CurrentUserResponse
decodeCurrentUserResponse res =
    case res of
        Ok success ->
            success

        Err err ->
            case err of
                Http.BadStatus response ->
                    case Decode.decodeString currentUserErrorDecoder response.body of
                        Err decodeError ->
                            CurrentUserParsingError

                        Ok parsedResponse ->
                            parsedResponse

                _ ->
                    CurrentUserHttpError err

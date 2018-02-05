module Helpers.User exposing (..)

import Dict exposing (Dict)
import Globals.Types exposing (Authentication)
import Helpers.Authentication exposing (authenticationHeaders)
import Helpers.Functions exposing (..)
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


userInfoDecoder : Decode.Decoder UserInfo
userInfoDecoder =
    DecodePipeline.decode
        (\id email firstName lastName ->
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



-- ////////// GET CURRENT USER //////////


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
        , expect =
            Http.expectJson <|
                Decode.map (\u -> CurrentUserSuccessResponse u) userInfoDecoder
        , headers = authenticationHeaders auth
        , method = "GET"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/users/current"
        , withCredentials = False
        }


currentUserErrorDecoder : Decode.Decoder CurrentUserResponse
currentUserErrorDecoder =
    errorDecoder
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


currentUserResponseDecode : Result Http.Error CurrentUserResponse -> CurrentUserResponse
currentUserResponseDecode res =
    responseDecode
        currentUserErrorDecoder
        CurrentUserInvalidResponse
        CurrentUserHttpError
        res



-- ////////// GET ALL USERS //////////


type ListUsersResponse
    = ListUsersSuccessResponse (Dict Int UserInfo)
    | ListUsersErrorResponse { error : ListUsersError, message : String }
    | ListUsersInvalidResponse
    | ListUsersHttpError Http.Error


type ListUsersError
    = ListUsersUnauthorizedError
    | ListUsersUnknownError String


listUsersRequest : Authentication -> Http.Request ListUsersResponse
listUsersRequest auth =
    Http.request
        { body = Http.emptyBody
        , expect =
            Http.expectJson <|
                Decode.map
                    (\users ->
                        ListUsersSuccessResponse <|
                            List.foldl
                                (\user dict ->
                                    Dict.insert user.id user dict
                                )
                                Dict.empty
                                users
                    )
                <|
                    Decode.list userInfoDecoder
        , headers = authenticationHeaders auth
        , method = "GET"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/users"
        , withCredentials = False
        }


listUsersErrorDecoder : Decode.Decoder ListUsersResponse
listUsersErrorDecoder =
    errorDecoder
        (\code message ->
            ListUsersErrorResponse
                { error =
                    case code of
                        "unauthorized" ->
                            ListUsersUnauthorizedError

                        errorCode ->
                            ListUsersUnknownError errorCode
                , message = message
                }
        )


listUsersResponseDecode : Result Http.Error ListUsersResponse -> ListUsersResponse
listUsersResponseDecode res =
    responseDecode
        listUsersErrorDecoder
        ListUsersInvalidResponse
        ListUsersHttpError
        res

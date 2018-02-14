port module Helpers.Authentication exposing (..)

import Json.Encode as Encode
import Globals.Types exposing (Authentication)
import Http
import Helpers.Functions exposing (..)

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

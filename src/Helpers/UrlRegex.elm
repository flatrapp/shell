module Helpers.UrlRegex exposing (..)

import Regex exposing (..)


-- "^([a-zA-Z]+)://([a-zA-Z0-9]+(?:\\.[a-zA-Z0-9]+)+)(?:\\:([0-9]+))?(\\/.*?)?(/*)?$"


domain : String
domain =
    "[a-zA-Z0-9\\-\\_]+(?:\\.[a-zA-Z0-9\\-\\_]+)*"


ipv4 : String
ipv4 =
    "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"


ipv6 : String
ipv6 =
    "\\[(?:(?:[0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|(?:[0-9a-fA-F]{1,4}:){1,7}:|(?:[0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|(?:[0-9a-fA-F]{1,4}:){1,5}(?::[0-9a-fA-F]{1,4}){1,2}|(?:[0-9a-fA-F]{1,4}:){1,4}(?::[0-9a-fA-F]{1,4}){1,3}|(?:[0-9a-fA-F]{1,4}:){1,3}(?::[0-9a-fA-F]{1,4}){1,4}|(?:[0-9a-fA-F]{1,4}:){1,2}(?::[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:(?:(?::[0-9a-fA-F]{1,4}){1,6})|:(?:(?::[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(?::[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(?:ffff(?::0{1,4}){0,1}:){0,1}(?:(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])|(?:[0-9a-fA-F]{1,4}:){1,4}:(?:(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9]))\\]"


host : String
host =
    "(" ++ ipv6 ++ "|" ++ ipv4 ++ "|" ++ domain ++ ")"


optProtocol : String
optProtocol =
    "(?:([a-zA-Z]+):\\/\\/)?"


optPort : String
optPort =
    "(?:\\:([0-9]+))?"


optLocation : String
optLocation =
    "(?:\\/?|(\\/.*?))?(?:\\/*)?"


urlRegexString : String
urlRegexString =
    "^" ++ optProtocol ++ host ++ optPort ++ optLocation ++ "$"


urlRegex : Regex
urlRegex =
    regex urlRegexString


type alias ResourceLocation =
    { protocol : String
    , host : String
    , portNumber : Maybe Int
    , location : String
    }


toMaybeInt : Int -> Maybe String -> Maybe Int
toMaybeInt default maybeString =
    case maybeString of
        Nothing ->
            Nothing

        Just string ->
            String.toInt string
                |> Result.withDefault default
                |> Just


toResourceLocation : String -> Maybe ResourceLocation
toResourceLocation url =
    case List.head <| find All urlRegex url of
        Nothing ->
            Nothing

        Just match ->
            case match.submatches of
                protocol :: host :: portNumber :: location :: _ ->
                    Just
                        { protocol = Maybe.withDefault "https" protocol
                        , host = Maybe.withDefault "" host
                        , portNumber = toMaybeInt -1 portNumber
                        , location = Maybe.withDefault "" location
                        }

                _ ->
                    Nothing


toUrlString : ResourceLocation -> String
toUrlString r =
    let
        _ =
            Debug.log "Resouce Location" r
    in
    case r.portNumber of
        Nothing ->
            r.protocol ++ "://" ++ r.host ++ r.location

        Just portNumber ->
            r.protocol ++ "://" ++ r.host ++ ":" ++ toString portNumber ++ r.location


checkUrlInput : String -> Maybe String
checkUrlInput =
    Maybe.map toUrlString << toResourceLocation

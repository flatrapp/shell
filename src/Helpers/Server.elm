port module Helpers.Server exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Time


requestTimeout : Float
requestTimeout =
    5 * Time.second

port saveServerInput : String -> Cmd msg
port clearServerInput : () -> Cmd msg


-- ////////// ////////// //////////


type alias ServerInfo =
    { version : String
    , name : String
    }


type ServerInfoResponse
    = ServerInfoSuccessResponse ServerInfo
    | ServerInfoInvalidResponse
    | ServerInfoHttpError Http.Error


serverInfoRequest : String -> Http.Request ServerInfoResponse
serverInfoRequest serverUrl =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson serverInfoSuccessDecoder
        , headers = []
        , method = "GET"
        , timeout = Just requestTimeout
        , url = serverUrl ++ "/info"
        , withCredentials = False
        }


serverInfoResponseDecode : Result Http.Error ServerInfoResponse -> ServerInfoResponse
serverInfoResponseDecode res =
    case res of
        Ok success ->
            success

        Err (Http.BadStatus _) ->
            ServerInfoInvalidResponse

        Err (Http.BadPayload _ _) ->
            -- The payload wasn't valid JSON / didn't have the expected fields
            ServerInfoInvalidResponse

        Err httpErr ->
            ServerInfoHttpError httpErr


serverInfoSuccessDecoder : Decode.Decoder ServerInfoResponse
serverInfoSuccessDecoder =
    DecodePipeline.decode
        (\version name ->
            ServerInfoSuccessResponse
                { version = version
                , name = name
                }
        )
        |> DecodePipeline.required "version" Decode.string
        |> DecodePipeline.required "name" Decode.string

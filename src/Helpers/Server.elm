module Helpers.Server exposing (..)

import Globals.Types exposing (ServerInfoResponse(..))
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Time


requestTimeout : Float
requestTimeout =
    5 * Time.second


type alias ServerInfo =
    { version : String
    }


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


decodeServerInfoResponse : Result Http.Error ServerInfoResponse -> ServerInfoResponse
decodeServerInfoResponse res =
    case res of
        Ok success ->
            success

        Err _ ->
            ServerInfoErrorResponse

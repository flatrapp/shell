module Helpers.Server exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Json.Encode as Encode
import Time


requestTimeout : Float
requestTimeout =
    5 * Time.second


type alias ServerInfo =
    { version : String
    }


type ServerInfoResponse
    = ServerInfoSuccessResponse ServerInfo
    | ServerInfoErrorResponse


serverInfoRequest : String -> Http.Request ServerInfoResponse
serverInfoRequest baseUrl =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson serverInfoSuccessDecoder
        , headers = []
        , method = "GET"
        , timeout = Just requestTimeout
        , url = baseUrl ++ "/info"
        , withCredentials = False
        }


serverInfoSuccessDecoder : Decode.Decoder ServerInfoResponse
serverInfoSuccessDecoder =
    DecodePipeline.decode
        (\version ->
            ServerInfoSuccessResponse
                { version = version
                }
        )
        |> DecodePipeline.required "version" Decode.string


decodeServerInfoResponse : Result Http.Error ServerInfoResponse -> ServerInfoResponse
decodeServerInfoResponse res =
    case res of
        Ok success ->
            success

        Err _ ->
            ServerInfoErrorResponse

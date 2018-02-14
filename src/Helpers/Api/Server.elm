port module Helpers.Api.Server exposing (..)

import Helpers.Functions exposing (flexibleResponseDecode, dateDecoder)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Time
import Time.DateTime as DateTime exposing (DateTime)


requestTimeout : Float
requestTimeout =
    5 * Time.second


port saveServerInput : String -> Cmd msg


port clearServerInput : () -> Cmd msg



-- ////////// ////////// //////////


type alias ServerInfo =
    { version : String
    , name : String
    , requestTime : DateTime
    , serverTime : DateTime
    }


type ServerInfoResponse
    = ServerInfoSuccessResponse ServerInfo
    | ServerInfoInvalidResponse
    | ServerInfoHttpError Http.Error


serverInfoRequest : String -> Time.Time -> Http.Request ServerInfoResponse
serverInfoRequest serverUrl time =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson <| serverInfoSuccessDecoder time
        , headers = []
        , method = "GET"
        , timeout = Just requestTimeout
        , url = serverUrl ++ "/info"
        , withCredentials = False
        }


serverInfoResponseDecode : Result Http.Error ServerInfoResponse -> ServerInfoResponse
serverInfoResponseDecode res =
    flexibleResponseDecode
        identity
        (\_ -> Err ()) -- Just throw a generic error here, automatically become invalid response
        ServerInfoInvalidResponse
        ServerInfoHttpError
        res


serverInfoSuccessDecoder : Time.Time -> Decode.Decoder ServerInfoResponse
serverInfoSuccessDecoder requestTime =
    DecodePipeline.decode
        (\version name serverTime ->
            ServerInfoSuccessResponse
                { version = version
                , name = name
                , requestTime = DateTime.fromTimestamp requestTime
                , serverTime = serverTime
                }
        )
        |> DecodePipeline.required "version" Decode.string
        |> DecodePipeline.required "name" Decode.string
        |> DecodePipeline.required "currentTime" dateDecoder

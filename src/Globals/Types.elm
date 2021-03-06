module Globals.Types exposing (..)

import Helpers.Api.Server exposing (ServerInfo, ServerInfoResponse)
import Http
import Navigation exposing (Location)
import Pages
import Time


type Msg
    = AppInitialized
    | TimeTick Time.Time
    | LocationChange Navigation.Location
    | SaveAuthentication Authentication
    | SaveServerInfo ServerInfo
    | SaveServerInput String
    | CheckRedirectLogin
    | RequestServerInfo Authentication
    | ServerInfoResponse (Result Http.Error ServerInfoResponse)
    | Logout


type alias Authentication =
    { serverUrl : String
    , token : String
    , tokenId : String
    , validUntil : Float
    }


type alias Model =
    { page : Pages.Page
    , location : Location
    , loginDestLocation : Location
    , time : Maybe Time.Time
    , timezoneOffset : Int
    , auth : Maybe Authentication
    , serverInfo : Maybe ServerInfo
    , serverInput : String
    , lastServerInfoUpdate : Float
    , version : String
    }


initialModel : Location -> String -> Int -> Model
initialModel location serverInput timezoneOffset =
    { page = Pages.parseLocation location
    , location = location
    , loginDestLocation = location
    , time = Nothing
    , timezoneOffset = timezoneOffset
    , auth = Nothing
    , serverInfo = Nothing
    , serverInput = serverInput
    , lastServerInfoUpdate = 0
    , version = "0.0.1"
    }

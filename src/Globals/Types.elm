module Globals.Types exposing (..)

import Navigation exposing (Location)
import Pages
import Time
import Http
import Helpers.Toast

type Msg
    = AppInitialized
    | TimeTick Time.Time
    | LocationChange Navigation.Location
    | Alert String
    | SaveAuthentication Authentication
    | SaveServerInfo ServerInfo
    | CheckRedirectLogin
    | RequestServerInfo Authentication
    | ServerInfoResponse (Result Http.Error ServerInfoResponse)
    | Logout

type ServerInfoResponse
    = ServerInfoSuccessResponse ServerInfo
    | ServerInfoErrorResponse

type alias Authentication =
    { serverUrl : String
    , token : String
    , tokenId : String
    , validUntil : Float
    }


type alias ServerInfo =
    { version : String
    , name : String
    }


type alias Model =
    { page : Pages.Page
    , location : Location
    , loginDestLocation : Location
    , time : Maybe Time.Time
    , timezoneOffset : Int
    , auth : Maybe Authentication
    , serverInfo : Maybe ServerInfo
    , lastServerInfoUpdate : Float
    , version : String
    }


initialModel : Location -> Int -> Model
initialModel location timezoneOffset =
    { page = Pages.parseLocation location
    , location = location
    , loginDestLocation = location
    , time = Nothing
    , timezoneOffset = timezoneOffset
    , auth = Nothing
    , serverInfo = Nothing
    , lastServerInfoUpdate = 0
    , version = "0.0.1"
    }

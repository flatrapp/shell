module Globals.Types exposing (..)

import Navigation exposing (Location)
import Pages
import Time


type Msg
    = AppInitialized
    | TimeTick Time.Time
    | LocationChange Navigation.Location
    | Alert String
    | SaveAuthentication Authentication
    | SaveServerInfo ServerInfo
    | CheckRedirectLogin
    | Logout


type alias Authentication =
    { token : String
    , tokenId : String
    , validUntil : Float
    }


type alias ServerInfo =
    { version : String
    }


type alias Model =
    { page : Pages.Page
    , location : Location
    , loginDestLocation : Location
    , apiBaseUrl : String
    , time : Maybe Time.Time
    , timezoneOffset : Int
    , auth : Maybe Authentication
    , serverInfo : Maybe ServerInfo
    , version : String
    }


initialModel : Location -> Int -> Model
initialModel location timezoneOffset =
    { page = Pages.parseLocation location
    , location = location
    , loginDestLocation = location
    , apiBaseUrl = "http://localhost:8000"
    , time = Nothing
    , timezoneOffset = timezoneOffset
    , auth = Nothing
    , serverInfo = Nothing
    , version = "0.0.1"
    }

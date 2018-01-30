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
    | CheckRedirectLogin
    | Logout


type alias Authentication =
    { token : String
    , tokenId : String
    , validUntil : Float
    }


type alias Model =
    { page : Pages.Page
    , location : Location
    , loginDestLocation : Location
    , apiBaseUrl : String
    , time : Maybe Time.Time
    , auth : Maybe Authentication
    }


initialModel : Location -> Model
initialModel location =
    { page = Pages.parseLocation location
    , location = location
    , loginDestLocation = location
    , apiBaseUrl = "http://localhost:8000"
    , time = Nothing
    , auth = Nothing
    }

module Model exposing (Model, Msg(..), initialModel)

import Bootstrap.Navbar
import Components.Dashboard exposing (Model, initialModel)
import Components.Login exposing (Model, initialModel)
import Globals exposing (Model)
import Navigation exposing (Location)


type alias Model =
    { login : Components.Login.Model
    , dashboard : Components.Dashboard.Model
    , globals : Globals.Model
    , navState : Bootstrap.Navbar.State
    }


initialModel : Location -> Bootstrap.Navbar.State -> Model
initialModel location navState =
    { login = Components.Login.initialModel
    , dashboard = Components.Dashboard.initialModel
    , globals = Globals.initialModel location
    , navState = navState
    }


type Msg
    = LocationChange Location
    | Login Components.Login.Msg
    | Dashboard Components.Dashboard.Msg
    | Globals Globals.Msg
    | NavbarEvent Bootstrap.Navbar.State

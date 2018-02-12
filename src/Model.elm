module Model exposing (Model, initialModel)

import Bootstrap.Navbar
import Components.Dashboard exposing (Model, initialModel)
import Components.Login exposing (Model, initialModel)
import Components.Settings exposing (Model, initialModel)
import Components.Signup exposing (Model, initialModel)
import Globals.Types
import Navigation exposing (Location)


type alias Model =
    { login : Components.Login.Model
    , signup : Components.Signup.Model
    , dashboard : Components.Dashboard.Model
    , settings : Components.Settings.Model
    , globals : Globals.Types.Model
    , navState : Bootstrap.Navbar.State
    }


initialModel : Location -> Bootstrap.Navbar.State -> Int -> String -> Model
initialModel location navState timezoneOffset serverInput =
    { login = Components.Login.initialModel serverInput
    , signup = Components.Signup.initialModel serverInput
    , dashboard = Components.Dashboard.initialModel
    , settings = Components.Settings.initialModel
    , globals = Globals.Types.initialModel location timezoneOffset
    , navState = navState
    }

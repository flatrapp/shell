module Model exposing (Model, initialModel)

import Bootstrap.Navbar
import Components.Dashboard exposing (Model, initialModel)
import Components.Login exposing (Model, initialModel)
import Components.Signup exposing (Model, initialModel)
import Globals.Types
import Navigation exposing (Location)


type alias Model =
    { login : Components.Login.Model
    , signup : Components.Signup.Model
    , dashboard : Components.Dashboard.Model
    , globals : Globals.Types.Model
    , navState : Bootstrap.Navbar.State
    }


initialModel : Location -> Bootstrap.Navbar.State -> Int -> Model
initialModel location navState timezoneOffset =
    { login = Components.Login.initialModel
    , signup = Components.Signup.initialModel
    , dashboard = Components.Dashboard.initialModel
    , globals = Globals.Types.initialModel location timezoneOffset
    , navState = navState
    }

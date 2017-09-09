module Models exposing (..)

import Authentication exposing (Authentication)
import Time exposing (Time)
import Bootstrap.Navbar as Navbar


type alias Model =
    { page : Page
    , navState : Navbar.State
    , auth : Maybe Authentication
    , time : Maybe Time
    , loginForm : LoginForm
    }


initialModel : Page -> Navbar.State -> Model
initialModel page navState =
    { page = page
    , navState = navState
    , auth = Nothing
    , time = Nothing
    , loginForm = initialLoginForm
    }



-- Routing types


type Page
    = Dashboard
    | Login
    | NotFound



-- Notifications


type NotificationKind
    = Info
    | Success
    | Warn
    | Error


type alias Notification =
    { title : String
    , message : String
    , kind : NotificationKind
    }



-- Form Types


type alias LoginForm =
    { email : String
    , password : String
    }


initialLoginForm : LoginForm
initialLoginForm =
    { email = ""
    , password = ""
    }


setLoginFormEmail : String -> Model -> Model
setLoginFormEmail email model =
    let
        loginForm =
            model.loginForm

        newLoginForm =
            { loginForm | email = email }
    in
        { model | loginForm = newLoginForm }


setLoginFormPassword : String -> Model -> Model
setLoginFormPassword password model =
    let
        loginForm =
            model.loginForm

        newLoginForm =
            { loginForm | password = password }
    in
        { model | loginForm = newLoginForm }

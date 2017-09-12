module Models exposing (..)

import Time exposing (Time)
import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar


type alias Model =
    { page : Page
    , location : Location
    , loginDestLocation : Location
    , navState : Navbar.State
    , auth : Maybe Authentication
    , time : Maybe Time
    , loginForm : LoginForm
    }


initialModel : Page -> Location -> Navbar.State -> Model
initialModel page location navState =
    { page = page
    , location = location
    , loginDestLocation = location
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



-- Backend Endpoints


type Endpoint
    = Auth



-- Authentication


type alias Email =
    String


type alias Password =
    String


type alias Token =
    String


type alias TokenId =
    String


type alias Authentication =
    { token : Token
    , tokenId : TokenId
    , validUntil : Time
    }


type alias UnwrappedAuthenticationResponse =
    { token : String
    , tokenId : String
    , validFor : Int
    }


type AuthenticationResponse
    = AuthenticationSuccessResponse
        { token : String
        , tokenId : String
        , validFor : Int
        }
    | AuthenticationErrorResponse
        { error : AuthenticationError
        , message : String
        }


type AuthenticationError
    = BadEmailPasswordError
    | UnknownAuthenticationError String



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

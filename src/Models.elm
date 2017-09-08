module Models exposing (..)

import Msgs exposing (Msg(..))

import Bootstrap.Navbar as Navbar

type alias Model =
    { page : Page
    , navState : Navbar.State
    }

initialModel : Page -> Navbar.State -> Model
initialModel page navState =
    { page = page
    , navState = navState
    }

-- Routing types

type Page
    = Dashboard
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

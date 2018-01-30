module Msg exposing (..)

import Bootstrap.Navbar
import Components.Dashboard
import Components.Login
import Globals.Types
import Navigation
import Time


type Msg
    = AppInitialized
    | TimeTick Time.Time
    | LocationChange Navigation.Location
    | Login Components.Login.Msg
    | Dashboard Components.Dashboard.Msg
    | Globals Globals.Types.Msg
    | NavbarEvent Bootstrap.Navbar.State
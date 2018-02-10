module Msg exposing (..)

import Bootstrap.Navbar
import Components.Dashboard
import Components.Login
import Components.Settings
import Components.Signup
import Globals.Types
import Navigation
import Time


type Msg
    = AppInitialized
    | TimeTick Time.Time
    | LocationChange Navigation.Location
    | NavbarEvent Bootstrap.Navbar.State
    | Globals Globals.Types.Msg
    | Login Components.Login.Msg
    | Signup Components.Signup.Msg
    | Dashboard Components.Dashboard.Msg
    | Settings Components.Settings.Msg

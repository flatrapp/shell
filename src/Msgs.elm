module Msgs exposing (..)

import Time exposing (Time)
import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar


type Msg
    = OnAppInitialized
    | OnTimeTick Time
    | OnLocationChange Location
    | OnNavbarEvent Navbar.State
    | OnLoginFormEmailChange String
    | OnLoginFormPasswordChange String

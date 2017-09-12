module Msgs exposing (..)

import Models exposing (..)
import Time exposing (Time)
import Navigation exposing (Location)
import Http
import Bootstrap.Navbar as Navbar


type Msg
    = OnAppInitialized
    | OnTimeTick Time
    | OnLocationChange Location
    | OnNavbarEvent Navbar.State
    | OnLoginFormEmailChange String
    | OnLoginFormPasswordChange String
    | RequestAuthentication -- Try login
    | OnAuthenticationResponse (Result Http.Error AuthenticationResponse)

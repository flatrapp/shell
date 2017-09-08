module Msgs exposing (..)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar

type Msg
    = OnLocationChange Location
    | OnNavbarEvent Navbar.State



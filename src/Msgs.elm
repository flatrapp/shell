module Msgs exposing (..)

import Http
import Models exposing (Player, PlayerLevel, PlayerLevelChange)
import RemoteData exposing (WebData)
import Navigation exposing (Location)

type Msg
    = OnFetchPlayers (WebData (List Player))
    | OnLocationChange Location
    | ChangePlayerLevel Player PlayerLevelChange
    | OnPlayerSave (Result Http.Error Player)


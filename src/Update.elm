module Update exposing (..)

import Msgs exposing (Msg(..))
import Models exposing (Model)
import Routing exposing (parseLocation)

import Debug

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msgs.OnNavbarEvent navState ->
            ( { model | navState = navState }, Cmd.none )
        Msgs.OnLocationChange location ->
            let
                newPage =
                    parseLocation location
                _ = Debug.log "Switching to page" newPage
            in
                ( { model | page = newPage }, Cmd.none )

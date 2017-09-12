module Main exposing (main)

import Models exposing (Model, initialModel, Page)
import Msgs exposing (Msg(..))
import Routing exposing (parseLocation)
import View exposing (view)
import Update exposing (update)
import Time exposing (second)
import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }



-- Application Init


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navState, navCmd ) =
            Navbar.initialState OnNavbarEvent

        ( model, cmd ) =
            update OnAppInitialized
                (initialModel (parseLocation location) location navState)
    in
        ( model, Cmd.batch [ navCmd, cmd ] )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second OnTimeTick
        , Navbar.subscriptions model.navState OnNavbarEvent
        ]

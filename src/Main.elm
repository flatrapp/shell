module Main exposing (main)

import Bootstrap.Navbar as Navbar
import Components.Dashboard exposing (Model, Msg)
import Components.Login exposing (Model, Msg)
import Globals exposing (..)
import Helpers.Authentication exposing (isAuthenticated)
import Model exposing (Model, Msg(..), initialModel)
import Navigation exposing (Location)
import Pages
import Time exposing (second)
import View exposing (view)


main : Program Flags Model.Model Model.Msg
main =
    Navigation.programWithFlags Model.LocationChange
        { init = init
        , view = View.view
        , update = updateWrapper
        , subscriptions = subscriptions
        }

type alias Flags =
  { auth : Maybe Globals.Authentication }

init : Flags -> Location -> ( Model.Model, Cmd Model.Msg )
init flags location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavbarEvent

        ( model, cmd ) =
            update (Globals AppInitialized) (Model.initialModel location navState)

        ( newGlobals, authSaveCmd ) =
            case flags.auth of
                Just auth ->
                    Globals.update (Globals.SaveAuthentication auth) model.globals
                Nothing ->
                    ( model.globals, Cmd.none )
    in
    { model | globals = newGlobals } ! [ navCmd, cmd, Cmd.map Globals authSaveCmd ]

updateWrapper : Model.Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
updateWrapper msg model =
    let
        (newModel, updateCmd) = update msg model
        (newGlobals, redirectCmd) = Globals.update Globals.CheckRedirectLogin newModel.globals
    in
        { newModel | globals = newGlobals } ! [ updateCmd, Cmd.map Globals redirectCmd ]

update : Model.Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
update msg model =
    case msg of
        Login lmsg ->
            let
                ( newModel, cmd, globalsCmd ) =
                    Components.Login.update lmsg model.login model.globals
            in
            { model | login = newModel } ! [ Cmd.map Login cmd, Cmd.map Globals globalsCmd ]

        Dashboard dmsg ->
            let
                ( newModel, cmd ) =
                    Components.Dashboard.update dmsg model.dashboard model.globals
            in
            { model | dashboard = newModel } ! [ Cmd.map Dashboard cmd ]

        Globals gmsg ->
            let
                ( newModel, cmd ) =
                    Globals.update gmsg model.globals
            in
            { model | globals = newModel } ! [ Cmd.map Globals cmd ]

        Model.LocationChange location ->
            let
                ( newModel, cmd ) =
                    Globals.update (Globals.LocationChange location) model.globals
            in
            { model | globals = newModel } ! [ Cmd.map Globals cmd ]

        NavbarEvent navState ->
            { model | navState = navState } ! []




subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    Sub.batch
        [ Sub.map Globals (Time.every second TimeTick)
        , Navbar.subscriptions model.navState NavbarEvent
        ]

module Main exposing (main)

import Bootstrap.Navbar as Navbar
import Components.Dashboard exposing (Model, Msg)
import Components.Login exposing (Model, Msg)
import Globals exposing (..)
import Globals.Types
import Helpers.Operators exposing ((:>))
import Model exposing (Model, initialModel)
import Msg exposing (Msg(..))
import Navigation exposing (Location)
import Time exposing (second)
import View exposing (view)
import Task

main : Program Flags Model.Model Msg.Msg
main =
    Navigation.programWithFlags Msg.LocationChange
        { init = init
        , view = View.view
        , update = updateWrapper
        , subscriptions = subscriptions
        }


type alias Flags =
    { auth : Maybe Globals.Types.Authentication }


init : Flags -> Location -> ( Model.Model, Cmd Msg.Msg )
init flags location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavbarEvent

        ( model, cmd ) =
            update Msg.AppInitialized (Model.initialModel location navState)

        ( newGlobals, authSaveGlobalsCmd, authSaveMainCmd ) =
            case flags.auth of
                Just auth ->
                    Globals.update (Globals.Types.SaveAuthentication auth) model.globals

                Nothing ->
                    ( model.globals, Cmd.none, Cmd.none )

        authSaveCmd =
            Cmd.batch [ Cmd.map Globals authSaveGlobalsCmd, authSaveMainCmd ]

        timeCmd = Task.perform Msg.TimeTick Time.now
    in
    { model | globals = newGlobals } ! [ navCmd, cmd, authSaveCmd, timeCmd ]


updateWrapper : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
updateWrapper msg model =
    let
        ( globals, redirectCmd, redirectMainCmd ) =
            Globals.update Globals.Types.CheckRedirectLogin model.globals

        ( newModel, updateCmd ) =
            update msg { model | globals = globals }
    in
    newModel ! [ Cmd.map Globals redirectCmd, redirectMainCmd, updateCmd ]


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.AppInitialized ->
            let
                ( newModel, msgs ) =
                    ( model, [] )
                        :> update (Msg.Globals Globals.Types.AppInitialized)
                        :> update (Msg.Login Components.Login.AppInitialized)
            in
            newModel ! msgs

        Msg.TimeTick time ->
            let
                -- TODO: Remove
                _ =
                    Debug.log "Login model" model.login

                ( newModel, msgs ) =
                    ( model, [] )
                        :> update (Msg.Globals (Globals.Types.TimeTick time))

                --:> update (Model.Login Components.Login.AppInitialized)
            in
            newModel ! msgs

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
                ( newModel, cmd, mainCmd ) =
                    Globals.update gmsg model.globals
            in
            { model | globals = newModel } ! [ Cmd.map Globals cmd, mainCmd ]

        Msg.LocationChange location ->
            update (Msg.Globals <| Globals.Types.LocationChange location) model

        NavbarEvent navState ->
            { model | navState = navState } ! []


subscriptions : Model.Model -> Sub Msg.Msg
subscriptions model =
    Sub.batch
        [ Time.every second Msg.TimeTick
        , Navbar.subscriptions model.navState NavbarEvent
        ]

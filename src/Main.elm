module Main exposing (main)

import Bootstrap.Navbar as Navbar
import Components.Dashboard exposing (Model, Msg)
import Components.Login exposing (Model, Msg)
import Components.Settings exposing (Model, Msg)
import Components.Signup exposing (Model, Msg)
import Globals exposing (..)
import Globals.Types
import Helpers.Operators exposing ((:>))
import Model exposing (Model, initialModel)
import Msg exposing (Msg(..))
import Navigation exposing (Location)
import Task
import Time exposing (second)
import View exposing (view)


main : Program Flags Model.Model Msg.Msg
main =
    Navigation.programWithFlags Msg.LocationChange
        { init = init
        , view = View.view
        , update = updateWrapper
        , subscriptions = subscriptions
        }


type alias Flags =
    { auth : Maybe Globals.Types.Authentication
    , timezoneOffset : Int
    , serverInput : String
    , invitationCode : Maybe String
    }


init : Flags -> Location -> ( Model.Model, Cmd Msg.Msg )
init flags location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavbarEvent

        ( model, cmd ) =
            update Msg.AppInitialized (Model.initialModel location navState flags.timezoneOffset flags.serverInput)

        ( newGlobals, authSaveGlobalsCmd, authSaveMainCmd ) =
            case flags.auth of
                Just auth ->
                    Globals.update (Globals.Types.SaveAuthentication auth) model.globals

                Nothing ->
                    ( model.globals, Cmd.none, Cmd.none )

        authSaveCmd =
            Cmd.batch [ Cmd.map Globals authSaveGlobalsCmd, authSaveMainCmd ]

        timeCmd =
            Task.perform Msg.TimeTick Time.now

        signupModel =
            model.signup

        newSignupModel =
            { signupModel | invitationCode = flags.invitationCode }
    in
    { model | globals = newGlobals, signup = newSignupModel } ! [ navCmd, cmd, authSaveCmd, timeCmd ]


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
                        :> update (Msg.Signup Components.Signup.AppInitialized)
            in
            newModel ! msgs

        Msg.TimeTick time ->
            let
                ( newModel, msgs ) =
                    ( model, [] )
                        :> update (Msg.Globals (Globals.Types.TimeTick time))
                        :> update (Msg.Dashboard (Components.Dashboard.TimeTick time))
            in
            newModel ! msgs

        Login lmsg ->
            let
                ( newModel, cmd, globalsCmd ) =
                    Components.Login.update lmsg model.login model.globals
            in
            { model | login = newModel } ! [ Cmd.map Login cmd, Cmd.map Globals globalsCmd ]

        Signup smsg ->
            let
                ( newModel, cmd, globalsCmd ) =
                    Components.Signup.update smsg model.signup model.globals
            in
            { model | signup = newModel } ! [ Cmd.map Signup cmd, Cmd.map Globals globalsCmd ]

        Dashboard dmsg ->
            let
                ( newModel, cmd, globalsCmd ) =
                    Components.Dashboard.update dmsg model.dashboard model.globals
            in
            { model | dashboard = newModel } ! [ Cmd.map Dashboard cmd, Cmd.map Globals globalsCmd ]

        Settings smsg ->
            let
                ( newModel, cmd, globalsCmd ) =
                    Components.Settings.update smsg model.settings model.globals
            in
            { model | settings = newModel } ! [ Cmd.map Settings cmd, Cmd.map Globals globalsCmd ]

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

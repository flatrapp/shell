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


-- This is the mail Elm application, which connects all custom code to the runtime
-- We use a Navigation-based program which accept flags initially sent from JavaScript


main : Program Flags Model.Model Msg.Msg
main =
    Navigation.programWithFlags Msg.LocationChange
        { init = init
        , view = View.view
        , update = updateWrapper
        , subscriptions = subscriptions
        }



-- The javascript gets the authentication from the localStorage, calculates a timezoneOffset value,
-- returns the default serverInput value from a GET parameter and a possibly an invitationCode


type alias Flags =
    { auth : Maybe Globals.Types.Authentication
    , timezoneOffset : Int
    , serverInput : String
    , invitationCode : Maybe String
    }



-- Initialize our application model, triggering all required commands


init : Flags -> Location -> ( Model.Model, Cmd Msg.Msg )
init flags location =
    let
        -- First, get the initial navbar state so that we don't have to care for that anymore
        ( navState, navCmd ) =
            Navbar.initialState NavbarEvent

        -- Let's get the initial model now so we can put our stuff into it
        initialModel =
            Model.initialModel location navState flags.timezoneOffset flags.serverInput

        -- The authentication needs to be saved before the first call to the main update function
        -- Else, it will redirect the user despite being logged in
        -- Later, he'll then be redirected to the dashboard  -> Not exactly what we want
        ( newGlobals, authSaveGlobalsCmd, authSaveMainCmd ) =
            case flags.auth of
                Just auth ->
                    Globals.update (Globals.Types.SaveAuthentication auth) initialModel.globals

                Nothing ->
                    ( initialModel.globals, Cmd.none, Cmd.none )

        authSaveCmd =
            Cmd.batch [ Cmd.map Globals authSaveGlobalsCmd, authSaveMainCmd ]

        -- Now that the authentication is saved in newGlobals, let's do our first call to the update function
        -- letting everyone know the app is initialized
        ( model, cmd ) =
            update Msg.AppInitialized { initialModel | globals = newGlobals }

        -- Request our first time tick so that we don't have to wait a second for the subscription
        timeCmd =
            Task.perform Msg.TimeTick Time.now

        -- Put the invitation code into the signup model, in case we got one passed via the URL
        signupModel =
            model.signup

        newSignupModel =
            { signupModel | invitationCode = flags.invitationCode }
    in
    { model | signup = newSignupModel } ! [ navCmd, cmd, authSaveCmd, timeCmd ]



-- In every update, we want to check whether we're still logged in and log ourselves out if necessary
-- This calles the primary update function


updateWrapper : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
updateWrapper msg model =
    let
        ( globals, redirectCmd, redirectMainCmd ) =
            Globals.update Globals.Types.CheckRedirectLogin model.globals

        ( newModel, updateCmd ) =
            update msg { model | globals = globals }
    in
    newModel ! [ Cmd.map Globals redirectCmd, redirectMainCmd, updateCmd ]



-- The primary application update function
-- All events arrive here, modify the model and return new commands to the Elm runtime


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
                        :> update (Msg.Login (Components.Login.TimeTick time))
                        :> update (Msg.Signup (Components.Signup.TimeTick time))
                        :> update (Msg.Settings (Components.Settings.TimeTick time))
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



-- These subscriptions get fulfilled by the Elm runtime and trigger the update function
-- with the provided message


subscriptions : Model.Model -> Sub Msg.Msg
subscriptions model =
    Sub.batch
        -- This triggers the update function every second, providing the current system time
        [ Time.every second Msg.TimeTick

        -- This is required by the Bootstrap Library
        , Navbar.subscriptions model.navState NavbarEvent
        ]

port module Globals exposing (..)

import Bootstrap.Navbar as Navbar
import Components.Dashboard as Dashboard
import Components.Login as Login
import Globals.Types exposing (Authentication, Model, Msg(..))
import Helpers.Alert exposing (sendAlert)
import Helpers.Authentication exposing (isAuthenticated)
import Helpers.Operators exposing ((!:), (!>))
import Msg
import Navigation exposing (Location)
import Pages exposing (Page(..), parseLocation)
import Task
import Time exposing (Time)


port clearAuthLocalStorage : () -> Cmd msg


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Msg.Msg )
update msg model =
    case msg of
        AppInitialized ->
            -- Do a location change with the initial parsed location when the app is initialized
            -- this will redirect the user immediately if necessary and send all the ViewState messages
            -- Sorta hack, but it works pretty well
            update (LocationChange model.location) model

        TimeTick newTime ->
            { model | time = Just newTime } !: []

        LocationChange location ->
            let
                -- First, try setting the destination in the model
                newModel =
                    { model | page = parseLocation location, location = location }

                -- Check if a redirect is necessary to reach the destination
                ( redirectModel, cmd, mainCmd ) =
                    checkRedirectLogin newModel Cmd.none

                -- Build all view-state commands (now that the final destination page is set)
                viewStateCmds =
                    viewStateMsgs redirectModel.page |> List.map send
            in
            redirectModel !> ( [ cmd ], mainCmd :: viewStateCmds )

        Alert msg ->
            model !: [ sendAlert msg ]

        SaveAuthentication auth ->
            { model | auth = Just auth } !: []

        SaveServerInfo serverInfo ->
            { model | serverInfo = Just serverInfo } !: []

        CheckRedirectLogin ->
            checkRedirectLogin model Cmd.none

        Logout ->
            { model | auth = Nothing } !: [ clearAuthLocalStorage () ]


checkRedirectLogin : Model -> Cmd Msg -> ( Model, Cmd Msg, Cmd Msg.Msg )
checkRedirectLogin model otherCmd =
    -- check if the user is authenticated
    if not (isAuthenticated model) && model.page /= Pages.LoginPage then
        -- not authenticated, switch to the login page
        { model
            | page = Pages.LoginPage
            , loginDestLocation = model.location
        }
            !: [ Navigation.modifyUrl "#login", otherCmd ]
    else if isAuthenticated model && model.page == Pages.LoginPage then
        { model
            | page = Pages.DashboardPage
        }
            !: [ Navigation.modifyUrl "#", otherCmd ]
    else
        model !: [ otherCmd ]


viewStateMsgs : Pages.Page -> List Msg.Msg
viewStateMsgs page =
    case page of
        LoginPage ->
            [ Msg.Login <| Login.ViewState True
            , Msg.Dashboard <| Dashboard.ViewState False
            ]

        DashboardPage ->
            [ Msg.Login <| Login.ViewState False
            , Msg.Dashboard <| Dashboard.ViewState True
            ]

        _ ->
            [ Msg.Login <| Login.ViewState False
            , Msg.Dashboard <| Dashboard.ViewState False
            ]

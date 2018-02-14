port module Globals exposing (..)

import Components.Dashboard as Dashboard
import Components.Login as Login
import Components.Settings as Settings
import Components.Signup as Signup
import Globals.Types exposing (Authentication, Model, Msg(..))
import Helpers.Authentication exposing (getValidAuth, isAuthenticated)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Api.Server exposing (ServerInfoResponse(..), serverInfoRequest, serverInfoResponseDecode)
import Helpers.Toast exposing (errorToast, simpleToast)
import Http
import Msg
import Navigation exposing (Location)
import Pages exposing (Page(..), parseLocation)
import Task
import Time


port clearAuthLocalStorage : () -> Cmd msg


serverInfoUpdateInterval : Float
serverInfoUpdateInterval =
    60 * Time.second


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
            let
                newModel =
                    { model | time = Just newTime }
            in
            case getValidAuth model of
                Nothing ->
                    newModel !: []

                Just auth ->
                    if model.lastServerInfoUpdate + serverInfoUpdateInterval < newTime then
                        update (RequestServerInfo auth) { newModel | lastServerInfoUpdate = newTime }
                    else
                        newModel !: []

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

        SaveAuthentication auth ->
            { model | auth = Just auth } !: []

        SaveServerInfo serverInfo ->
            { model | serverInfo = Just serverInfo } !: []

        CheckRedirectLogin ->
            checkRedirectLogin model Cmd.none

        ServerInfoResponse res ->
            case serverInfoResponseDecode res of
                ServerInfoSuccessResponse info ->
                    model !: [ send <| Globals.Types.SaveServerInfo info ]

                ServerInfoInvalidResponse ->
                    { model | serverInfo = Nothing }
                        !: [ errorToast "Communication Error" <|
                                "Received an invalid response while trying to get the server information."
                                    ++ "This most likely means something is wrong with the server or server URL."
                           ]

                ServerInfoHttpError _ ->
                    { model | serverInfo = Nothing }
                        !: [ errorToast "Communication Error" "There was an error while trying to get the server information." ]

        RequestServerInfo auth ->
            case model.time of
                Nothing -> model !: []
                Just time ->
                  model !: [ Http.send ServerInfoResponse (serverInfoRequest auth.serverUrl time) ]

        Logout ->
            checkRedirectLogin { model | auth = Nothing } (clearAuthLocalStorage ())


checkRedirectLogin : Model -> Cmd Msg -> ( Model, Cmd Msg, Cmd Msg.Msg )
checkRedirectLogin model otherCmd =
    -- check if the user is authenticated
    if not (isAuthenticated model) && model.page /= Pages.LoginPage && model.page /= Pages.SignupPage then
        -- not authenticated, switch to the login page
        { model
            | page = Pages.LoginPage
            , loginDestLocation = model.location
        }
            !: [ Navigation.modifyUrl "#login", otherCmd ]
    else if isAuthenticated model && (model.page == Pages.LoginPage || model.page == Pages.SignupPage) then
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
            , Msg.Signup <| Signup.ViewState False
            , Msg.Settings <| Settings.ViewState Nothing
            ]

        DashboardPage ->
            [ Msg.Login <| Login.ViewState False
            , Msg.Dashboard <| Dashboard.ViewState True
            , Msg.Signup <| Signup.ViewState False
            , Msg.Settings <| Settings.ViewState Nothing
            ]

        SignupPage ->
            [ Msg.Login <| Login.ViewState False
            , Msg.Dashboard <| Dashboard.ViewState False
            , Msg.Signup <| Signup.ViewState True
            , Msg.Settings <| Settings.ViewState Nothing
            ]

        SettingsPage subPage ->
            [ Msg.Login <| Login.ViewState False
            , Msg.Dashboard <| Dashboard.ViewState False
            , Msg.Signup <| Signup.ViewState False
            , Msg.Settings <| Settings.ViewState <| Just subPage
            ]

        _ ->
            [ Msg.Login <| Login.ViewState False
            , Msg.Dashboard <| Dashboard.ViewState False
            , Msg.Signup <| Signup.ViewState False
            , Msg.Settings <| Settings.ViewState Nothing
            ]

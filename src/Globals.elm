port module Globals exposing (..)

import Bootstrap.Navbar as Navbar
import Helpers.Alert exposing (sendAlert)
import Navigation exposing (Location)
import Pages exposing (Page, parseLocation)
import Time exposing (Time)


port clearAuthLocalStorage : () -> Cmd msg


type alias Model =
    { page : Page
    , location : Location
    , loginDestLocation : Location
    , apiBaseUrl : String
    , time : Maybe Time
    , auth : Maybe Authentication
    }


initialModel : Location -> Model
initialModel location =
    { page = Pages.parseLocation location
    , location = location
    , loginDestLocation = location
    , apiBaseUrl = "http://localhost:8000"
    , time = Nothing
    , auth = Nothing
    }


type Msg
    = AppInitialized
    | TimeTick Time
    | LocationChange Location
    | Alert String
    | SaveAuthentication Authentication
    | CheckRedirectLogin
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppInitialized ->
            model ! []

        TimeTick newTime ->
            let
                _ =
                    Debug.log "Globals" model
            in
            { model | time = Just newTime } ! []

        LocationChange location ->
            { model
                | page = parseLocation location
                , location = location
            }
                ! []

        Alert msg ->
            model ! [ sendAlert msg ]

        SaveAuthentication auth ->
            { model | auth = Just auth } ! []

        CheckRedirectLogin ->
            checkRedirectLogin model Cmd.none

        Logout ->
            { model | auth = Nothing } ! [ clearAuthLocalStorage () ]


isAuthenticated : Model -> Bool
isAuthenticated globals =
    case globals.auth of
        Just a ->
            True

        Nothing ->
            False


checkRedirectLogin : Model -> Cmd Msg -> ( Model, Cmd Msg )
checkRedirectLogin model otherCmd =
    -- check if the user is authenticated
    if not (isAuthenticated model) && model.page /= Pages.LoginPage then
        -- not authenticated, switch to the login page
        ( { model
            | page = Pages.LoginPage
            , loginDestLocation = model.location
          }
        , Cmd.batch [ Navigation.modifyUrl "#login", otherCmd ]
        )
    else if isAuthenticated model && model.page == Pages.LoginPage then
        ( { model
            | page = Pages.DashboardPage
          }
        , Cmd.batch [ Navigation.modifyUrl "#", otherCmd ]
        )
    else
        ( model, otherCmd )


type alias Authentication =
    { token : String
    , tokenId : String
    , validUntil : Float
    }

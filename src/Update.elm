module Update exposing (..)

import Msgs exposing (Msg(..))
import Models exposing (..)
import Routing exposing (parseLocation, loginPath)
import Authentication exposing (checkAuthenticated)
import Navigation
import Debug


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnAppInitialized ->
            -- Check if the user is authenticated on application load
            -- as OnLocationChange doesn't get triggered directly
            checkRedirectLogin model Cmd.none

        Msgs.OnTimeTick newTime ->
            -- Check if the user is still authenticated and go to login if not
            -- Also store time in model
            checkRedirectLogin { model | time = Just newTime } Cmd.none

        Msgs.OnNavbarEvent navState ->
            ( { model | navState = navState }, Cmd.none )

        Msgs.OnLocationChange location ->
            checkRedirectLogin { model | page = parseLocation location } Cmd.none

        Msgs.OnLoginFormEmailChange email ->
            ( setLoginFormEmail email model, Cmd.none )

        Msgs.OnLoginFormPasswordChange password ->
            ( setLoginFormPassword password model, Cmd.none )


checkRedirectLogin : Model -> Cmd Msg -> ( Model, Cmd Msg )
checkRedirectLogin model cmd =
    -- check if the user is authenticated
    if not (checkAuthenticated model.time model.auth) && model.page /= Login then
        -- not authenticated, switch to the login page
        ( { model | page = Login }, Cmd.batch [ Navigation.newUrl loginPath, cmd ] )
    else
        ( model, cmd )

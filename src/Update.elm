module Update exposing (..)

import Msgs exposing (Msg(..))
import Models exposing (..)
import Commands
import Routing exposing (parseLocation, loginPath)
import Authentication exposing (checkAuthenticated, fromAuthenticationResponse)
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
            let
                _ =
                    Debug.log "Model" model
            in
                checkRedirectLogin { model | time = Just newTime } Cmd.none

        Msgs.OnNavbarEvent navState ->
            ( { model | navState = navState }, Cmd.none )

        Msgs.OnLocationChange location ->
            checkRedirectLogin
                { model
                    | page = parseLocation location
                    , location = location
                }
                Cmd.none

        Msgs.OnLoginFormEmailChange email ->
            ( setLoginFormEmail email model, Cmd.none )

        Msgs.OnLoginFormPasswordChange password ->
            ( setLoginFormPassword password model, Cmd.none )

        Msgs.RequestAuthentication ->
            ( model
            , Commands.requestAuthCmd
                model.loginForm.email
                model.loginForm.password
            )

        Msgs.OnAuthenticationResponse (Ok authResponse) ->
            handleAuthenticationResponse authResponse model

        Msgs.OnAuthenticationResponse (Err authRequestErr) ->
            let
                _ =
                    Debug.log "auth request error" authRequestErr
            in
                ( model, Cmd.none )


handleAuthenticationResponse : AuthenticationResponse -> Model -> ( Model, Cmd Msg )
handleAuthenticationResponse authRes model =
    case authRes of
        AuthenticationSuccessResponse unwrappedRes ->
            createSaveAuthentication unwrappedRes model

        AuthenticationErrorResponse errorRes ->
            let
                logError =
                    Debug.log "AuthError"

                _ =
                    case errorRes.error of
                        BadEmailPasswordError ->
                            logError "bad email or password"

                        UnknownAuthenticationError s ->
                            logError "unknown error - " ++ s
            in
                ( model, Cmd.none )



-- Creates and saves an authentication in case of success, does nothing in case of error


createSaveAuthentication : UnwrappedAuthenticationResponse -> Model -> ( Model, Cmd Msg )
createSaveAuthentication res model =
    case model.time of
        Just time ->
            let
                destinationHash =
                    case model.loginDestLocation.hash of
                        "" ->
                            "#"

                        x ->
                            x
            in
                ( { model | auth = fromAuthenticationResponse time res }
                , Navigation.newUrl destinationHash
                )

        Nothing ->
            ( model, Cmd.none )


checkRedirectLogin : Model -> Cmd Msg -> ( Model, Cmd Msg )
checkRedirectLogin model otherCmd =
    -- check if the user is authenticated
    if not (checkAuthenticated model.time model.auth) && model.page /= Login then
        -- not authenticated, switch to the login page
        ( { model | page = Login, loginDestLocation = model.location }
        , Cmd.batch [ Navigation.modifyUrl loginPath, otherCmd ]
        )
    else
        ( model, otherCmd )

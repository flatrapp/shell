module Components.Login exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input exposing (onInput, value)
import Components.ServerInput as ServerInput
import Globals.Types
import Helpers.Api.Authentication exposing (..)
import Helpers.Authentication exposing (saveAuthentication)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Toast exposing (errorToast)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (for, href, id, pattern, required, style)
import Html.Events exposing (onSubmit)
import Http
import Navigation
import Task
import Time exposing (Time, second)

type alias Model =
    { state : State
    , email : String
    , password : String
    , serverInput : ServerInput.Model
    }


initialModel : String -> Model
initialModel serverInput =
    { state = LoginForm
    , email = ""
    , password = ""
    , serverInput = ServerInput.initialModel serverInput
    }


type State
    = LoginForm
    | LoginPending

type Msg
    = AppInitialized
    | RequestAuthentication
    | EmailChange String
    | PasswordChange String
    | AuthResponse (Result Http.Error AuthenticationResponse)
    | ViewState Bool
    | TimeTick Time.Time
    | ServerInputMsg ServerInput.Msg


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        AppInitialized ->
            model !: []

        TimeTick time ->
            let
                ( newModel, cmd ) =
                    ServerInput.update (ServerInput.TimeTick time) model.serverInput globals
            in
            { model | serverInput = newModel } !: [ Cmd.map ServerInputMsg cmd ]

        ServerInputMsg msg ->
            let
                ( newModel, cmd ) =
                    ServerInput.update msg model.serverInput globals
            in
            { model | serverInput = newModel } !: [ Cmd.map ServerInputMsg cmd ]

        ViewState state ->
            case state of
                False ->
                    initialModel (ServerInput.getPrefilledInput model.serverInput) !: []

                True ->
                    model !: []

        EmailChange email ->
            { model | email = email } !> ( [], [] )

        PasswordChange password ->
            { model | password = password } !: []

        RequestAuthentication ->
            case ServerInput.getUrl model.serverInput of
                Nothing ->
                    model !: [ errorToast "Inputs invalid" "The entered server could not be converted to a well-formed url." ]

                Just serverUrl ->
                    { model | state = LoginPending }
                        !: [ Http.send AuthResponse (authRequest serverUrl model.email model.password) ]

        AuthResponse res ->
            let
                ( cmd, globalsCmd ) =
                    handleAuthResponse globals
                        (\auth ->
                            -- ( send <| SaveServerInput model.serverInput
                            ( Cmd.none, send <| Globals.Types.RequestServerInfo auth )
                        )
                        res
            in
            ( { model | state = LoginForm }, cmd, globalsCmd )


-- This function takes a lot of the work from the update function
-- and manages the rather complex information flow when an
-- authentication response is received
handleAuthResponse :
    Globals.Types.Model
    -> (Globals.Types.Authentication -> ( Cmd Msg, Cmd Globals.Types.Msg ))
    -> Result Http.Error AuthenticationResponse
    -> ( Cmd Msg, Cmd Globals.Types.Msg )
handleAuthResponse globals successCmdFn res =
    case authResponseDecode res of
        AuthenticationSuccessResponse authSuccess ->
            case globals.time of
                -- We need the system time to calculate the remaing time
                -- for which the token is valid
                Nothing ->
                    -- This should not happen
                    ( errorToast "Internal Error" <|
                        "The authentication can't be saved because no TimeTick has been received."
                            ++ "<br />If you see this error in the wild, it means that I probably fucked up real bad."
                    , Cmd.none
                    )

                Just time ->
                    let
                        auth =
                            toAuthentication authSuccess time

                        ( successCmd, successGlobalsCmd ) =
                            successCmdFn auth

                        -- Navigate back to the URL, we were originally redirected from
                        -- If it was just an empty URL, go to the dashboard
                        destinationHash =
                            case globals.loginDestLocation.hash of
                                "" ->
                                    "#"

                                x ->
                                    x
                    in
                    ( successCmd
                    , Cmd.batch
                        [ saveAuthentication auth
                        , successGlobalsCmd
                        , Navigation.newUrl destinationHash
                        ]
                    )

        -- Plain old error handling by sending toasts here
        AuthenticationErrorResponse authErr ->
            ( case authErr.error of
                BadCredentialsError ->
                    errorToast "Invalid credentials" "Your email or password is wrong. Please check your inputs!"

                EmailNotVerifiedError ->
                    errorToast "Email not verified" "You haven't verified your email"

                UnknownAuthenticationError _ ->
                    errorToast "Unknown error" authErr.message
            , Cmd.none
            )

        _ ->
            -- TODO: Handle network errors seperately
            ( errorToast "Communication Error" "An unknown error occured while communicating with the server.", Cmd.none )



-- A helper to create the attribute list for inputs
-- This should improve the readability in the view function
inputAttrs : Bool -> Bool -> String -> String -> (String -> Msg) -> List (Input.Option Msg)
inputAttrs enabled requiredVal id val msg =
    [ Input.disabled <| not enabled
    , Input.attrs [ required requiredVal ]
    , Input.id id
    , value val
    , onInput msg
    ]


view : Model -> Globals.Types.Model -> Html Msg
view model globals =
    let
        -- Disable the login form if a request is in progress
        formEnable =
            case model.state of
                LoginForm ->
                    True

                LoginPending ->
                    False

        -- Pre-definied attributes for Inputs and Buttons
        dInput =
            Input.disabled <| not formEnable

        dButton =
            Button.disabled <| not formEnable
    in
    div []
        [ h1 [ style [ ( "margin-bottom", "1.2em" ) ] ] [ text "Login" ]
        , Form.form [ id "login-form", onSubmit RequestAuthentication ]
            [ Html.map ServerInputMsg <| ServerInput.view model.serverInput formEnable
            , Form.group []
                [ Form.label [ for "email" ] [ text "E-Mail:" ]
                , Input.email <| inputAttrs formEnable True "email" model.email EmailChange
                ]
            , Form.group []
                [ Form.label [ for "password" ] [ text "Password:" ]
                , Input.password <| inputAttrs formEnable True "password" model.password PasswordChange
                ]
            , Form.group []
                [ Form.help []
                    [ text "Your password will never be stored in plaintext. "
                    , text "Please make sure that this website is visited "
                    , text "over a secure HTTPS connection!"
                    ]
                ]
            , div [ style [ ( "float", "clear" ) ] ]
                [ div [ style [ ( "float", "left" ) ] ] [ Button.button [ dButton, Button.primary ] [ text "Login" ] ]
                , div [ style [ ( "float", "right" ) ] ] [ Button.linkButton [ dButton, Button.secondary, Button.attrs [ href "#signup" ] ] [ text "Signup for a new account" ] ]
                ]
            ]
        ]

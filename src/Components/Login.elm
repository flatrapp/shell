module Components.Login exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input exposing (onInput, value)
import Globals.Types
import Helpers.Api.Authentication exposing (..)
import Helpers.Authentication exposing (saveAuthentication)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Api.Server exposing (saveServerInput)
import Helpers.Toast exposing (errorToast)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (for, href, id, style)
import Html.Events exposing (onSubmit)
import Http
import Navigation
import Task


type alias Model =
    { state : State
    , email : String
    , password : String
    , serverInputDefault : String
    , serverInput : String
    , serverUrl : String
    , inputsValid : Bool
    , inputsValidMsg : String
    }


initialModel : String -> Model
initialModel serverInput =
    { state = LoginForm
    , email = ""
    , password = ""
    , serverInputDefault = serverInput
    , serverInput = serverInput
    , serverUrl = ""
    , inputsValid = False
    , inputsValidMsg = ""
    }


type State
    = LoginForm
    | LoginPending


type Msg
    = AppInitialized
    | RequestAuthentication
    | EmailChange String
    | PasswordChange String
    | ServerInputChange String
    | AuthResponse (Result Http.Error AuthenticationResponse)
    | SaveServerInput String
    | ViewState Bool


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        AppInitialized ->
            model !: []

        ViewState state ->
            case state of
                False ->
                    initialModel model.serverInputDefault !: []

                True ->
                    model !: []

        EmailChange email ->
            { model | email = email } !> ( [], [] )

        PasswordChange password ->
            { model | password = password } !: []

        RequestAuthentication ->
            let
                ( inputsValid, inputsValidMsg, serverUrl ) =
                    validateInputs model

                newModel =
                    { model
                        | serverUrl = serverUrl
                        , inputsValid = inputsValid
                        , inputsValidMsg = inputsValidMsg
                    }
            in
            if inputsValid then
                { newModel | state = LoginPending }
                    !: [ Http.send AuthResponse (authRequest newModel.serverUrl newModel.email newModel.password) ]
            else
                newModel !: [ errorToast "Inputs invalid" "One or more inputs are invalid.<br />Please correct the values and try again." ]

        AuthResponse res ->
            let
                ( cmd, globalsCmd ) =
                    handleAuthResponse globals
                        model.serverUrl
                        (\auth ->
                            ( send <| SaveServerInput model.serverInput
                            , send <| Globals.Types.RequestServerInfo auth
                            )
                        )
                        res
            in
            ( { model | state = LoginForm }, cmd, globalsCmd )

        SaveServerInput input ->
            { model | serverInputDefault = input } !: [ saveServerInput input ]

        ServerInputChange serverInput ->
            let
                ( inputsValid, inputsValidMsg, serverUrl ) =
                    validateInputs { model | serverInput = serverInput }
            in
            { model
                | serverInput = serverInput
                , serverUrl = serverUrl
                , inputsValid = inputsValid
                , inputsValidMsg = inputsValidMsg
            }
                !: []


serverInputToUrl : String -> Maybe String
serverInputToUrl input =
    if String.length input < 1 then
        Nothing
    else
        Just input


validateInputs : Model -> ( Bool, String, String )
validateInputs model =
    case serverInputToUrl model.serverInput of
        Nothing ->
            ( False, "Server input invalid", "" )

        Just serverUrl ->
            ( True, "", serverUrl )


handleAuthResponse :
    Globals.Types.Model
    -> String
    -> (Globals.Types.Authentication -> ( Cmd Msg, Cmd Globals.Types.Msg ))
    -> Result Http.Error AuthenticationResponse
    -> ( Cmd Msg, Cmd Globals.Types.Msg )
handleAuthResponse globals serverUrl successCmdFn res =
    case authResponseDecode res of
        AuthenticationSuccessResponse authSuccess ->
            case globals.time of
                Nothing ->
                    ( errorToast "Internal Error" <|
                        "The authentication can't be saved because no TimeTick has been received."
                            ++ "<br />If you see this error in the wild, it means that I probably fucked up real bad."
                    , Cmd.none
                    )

                Just time ->
                    let
                        auth =
                            toAuthentication serverUrl authSuccess time

                        ( successCmd, successGlobalsCmd ) =
                            successCmdFn auth

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


view : Model -> Globals.Types.Model -> Html Msg
view model globals =
    let
        formEnable =
            case model.state of
                LoginForm ->
                    True

                LoginPending ->
                    False

        dInput =
            Input.disabled <| not formEnable

        dButton =
            Button.disabled <| not formEnable
    in
    div []
        [ h1 [ style [ ( "margin-bottom", "1.2em" ) ] ] [ text "Login" ]
        , Form.form [ id "login-form", onSubmit RequestAuthentication ]
            [ Form.group []
                [ Form.label [ for "server" ] [ text "Server (There's one per flat):" ]
                , Input.text [ dInput, Input.id "server", onInput ServerInputChange, value model.serverInput ]
                ]
            , Form.group []
                [ Form.label [ for "email" ] [ text "E-Mail:" ]
                , Input.email [ dInput, Input.id "email", onInput EmailChange, value model.email ]
                ]
            , Form.group []
                [ Form.label [ for "password" ] [ text "Password:" ]
                , Input.password [ dInput, Input.id "password", onInput PasswordChange, value model.password ]
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

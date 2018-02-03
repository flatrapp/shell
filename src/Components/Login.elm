module Components.Login exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input exposing (onInput, value)
import Globals.Types
import Helpers.Authentication exposing (..)
import Helpers.Operators exposing ((!:), (!>))
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (for, href, id, style)
import Html.Events exposing (onSubmit)
import Http
import Navigation
import Regex exposing (Regex)
import Task


type alias Model =
    { email : String
    , password : String
    , serverInputDefault : String
    , serverInput : String
    , serverUrl : String
    , inputsValid : Bool
    , inputsValidMsg : String
    }


initialModel : String -> Model
initialModel serverInput =
    { email = ""
    , password = ""
    , serverInputDefault = serverInput
    , serverInput = serverInput
    , serverUrl = ""
    , inputsValid = False
    , inputsValidMsg = ""
    }


type Msg
    = AppInitialized
    | RequestAuthentication
    | EmailChange String
    | PasswordChange String
    | ServerInputChange String
    | AuthResponse (Result Http.Error AuthenticationResponse)
    | ViewState Bool


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    let _ = Debug.log "LOGIN MODEL" model
    in
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
            model !: [ Http.send AuthResponse (authRequest model.serverUrl model.email model.password) ]

        AuthResponse res ->
            model
                !> ( []
                   , [ handleAuthResponse globals
                        model.serverUrl
                        (\auth -> send <| Globals.Types.RequestServerInfo auth)
                        res
                     ]
                   )

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
    -> (Globals.Types.Authentication -> Cmd Globals.Types.Msg)
    -> Result err AuthenticationResponse
    -> Cmd Globals.Types.Msg
handleAuthResponse globals serverUrl successCmdFn res =
    case res of
        Err err ->
            send (Globals.Types.Alert "Http.Error")

        Ok authResponse ->
            case authResponse of
                AuthenticationSuccessResponse authSuccess ->
                    case globals.time of
                        Nothing ->
                            send (Globals.Types.Alert "No time tick received yet")

                        Just time ->
                            let
                                auth =
                                    toAuthentication serverUrl authSuccess time

                                successCmd =
                                    successCmdFn auth

                                destinationHash =
                                    case globals.loginDestLocation.hash of
                                        "" ->
                                            "#"

                                        x ->
                                            x
                            in
                            Cmd.batch
                                [ saveAuthentication auth
                                , successCmd
                                , Navigation.newUrl destinationHash
                                ]

                -- saveAuthentication auth
                AuthenticationErrorResponse authError ->
                    send (Globals.Types.Alert authError.message)


view : Model -> Globals.Types.Model -> Html Msg
view model globals =
    div []
        [ h1 [ style [ ( "margin-bottom", "1.2em" ) ] ] [ text "Login" ]
        , Form.form [ id "login-form", onSubmit RequestAuthentication ]
            [ Form.group []
                [ Form.label [ for "server" ] [ text "Server: (There's one server per flat)" ]
                , Input.text [ Input.id "server", onInput ServerInputChange, value model.serverInput ]
                ]
            , Form.group []
                [ Form.label [ for "email" ] [ text "E-Mail:" ]
                , Input.email [ Input.id "email", onInput EmailChange, value model.email ]
                ]
            , Form.group []
                [ Form.label [ for "password" ] [ text "Password:" ]
                , Input.password [ Input.id "password", onInput PasswordChange, value model.password ]
                ]
            , Form.group []
                [ Form.help []
                    [ text "Your password will never be stored in plaintext. "
                    , text "Please make sure that this website is visited "
                    , text "over a secure HTTPS connection!"
                    ]
                ]
            , div [ style [ ( "float", "clear" ) ] ]
                [ div [ style [ ( "float", "left" ) ] ] [ Button.button [ Button.primary ] [ text "Login" ] ]
                , div [ style [ ( "float", "right" ) ] ] [ Button.linkButton [ Button.secondary, Button.attrs [ href "#signup" ] ] [ text "Signup for a new account" ] ]
                ]
            ]
        ]

module Components.Login exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input exposing (onInput)
import Globals exposing (Model)
import Helpers.Authentication exposing (..)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (for, style)
import Html.Events exposing (onSubmit)
import Http
import Task
import Navigation

type alias Model =
    { email : String
    , password : String
    }


initialModel : Model
initialModel =
    { email = ""
    , password = ""
    }


type Msg
    = RequestAuthentication
    | EmailChange String
    | PasswordChange String
    | AuthResponse (Result Http.Error AuthenticationResponse)


(!) : model -> List (Cmd msg) -> ( model, Cmd msg, Cmd globalsMsg )
(!) model msgs =
    ( model, Cmd.batch msgs, Cmd.batch [] )


(!>) : model -> ( List (Cmd msg), List (Cmd globalsMsg) ) -> ( model, Cmd msg, Cmd globalsMsg )
(!>) model ( msgs, globalsMsgs ) =
    ( model, Cmd.batch msgs, Cmd.batch globalsMsgs )


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> Globals.Model -> ( Model, Cmd Msg, Cmd Globals.Msg )
update msg model globals =
    case msg of
        RequestAuthentication ->
            let
                req =
                    authRequest globals.apiBaseUrl model.email model.password
            in
            model !> ( [ Http.send AuthResponse req ], [] )

        EmailChange email ->
            { model | email = email } !> ( [], [] )

        PasswordChange password ->
            { model | password = password } ! []

        AuthResponse res ->
            model !> ( [], [ handleAuthResponse globals res ] )


handleAuthResponse : Globals.Model -> Result err AuthenticationResponse -> Cmd Globals.Msg
handleAuthResponse globals res =
    case res of
        Err err ->
            send (Globals.Alert "Http.Error")

        Ok authResponse ->
            case authResponse of
                AuthenticationSuccessResponse auth ->
                    case globals.time of
                        Nothing ->
                            send (Globals.Alert "No time tick received yet")
                        Just time ->
                            let destinationHash =
                                case globals.loginDestLocation.hash of
                                    "" ->
                                        "#"

                                    x ->
                                        x
                            in
                                Cmd.batch
                                    [ saveAuthentication auth time
                                    , Navigation.newUrl destinationHash
                                    ]

                -- saveAuthentication auth
                AuthenticationErrorResponse authError ->
                    send (Globals.Alert authError.message)


view : Model -> Globals.Model -> Html Msg
view model globals =
    div []
        [ h1 [ style [ ( "margin-bottom", "1.2em" ) ] ] [ text "Login" ]
        , Form.form [ onSubmit RequestAuthentication ]
            [ Form.group []
                [ Form.label [ for "email" ] [ text "E-Mail:" ]
                , Input.email [ Input.id "email", onInput EmailChange ]
                ]
            , Form.group []
                [ Form.label [ for "password" ] [ text "Password:" ]
                , Input.password [ Input.id "password", onInput PasswordChange ]
                ]
            , Form.group []
                [ Form.help []
                    [ text "Your password will never be stored in plaintext. "
                    , text "Please make sure that this website is visited "
                    , text "over a secure HTTPS connection!"
                    ]
                ]
            , Button.button [ Button.primary ] [ text "Login" ]
            ]
        ]

module Components.Signup exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input exposing (onInput)
import Globals.Types
import Helpers.Authentication exposing (..)
import Helpers.Operators exposing ((!:), (!>))
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (for, href, style)
import Html.Events exposing (onSubmit)
import Http
import Task


type alias Model =
    { email : String
    , firstName : String
    , lastName : String
    , password : String
    , passwordRepeat : String
    , serverInput : String
    , serverUrl : String
    }


initialModel : Model
initialModel =
    { email = ""
    , firstName = ""
    , lastName = ""
    , password = ""
    , passwordRepeat = ""
    , serverInput = ""
    , serverUrl = "http://localhost:8080"
    }


type Msg
    = AppInitialized
    | ViewState Bool
    | FirstNameChange String
    | LastNameChange String
    | EmailChange String
    | PasswordChange String
    | PasswordRepeatChange String
    | RequestSignup
    | SignupResponse (Result Http.Error SignupResponse)


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
                    initialModel !: []

                True ->
                    model !: []

        FirstNameChange firstName ->
            { model | firstName = firstName } !> ( [], [] )

        LastNameChange lastName ->
            { model | lastName = lastName } !: []

        EmailChange email ->
            { model | email = email } !> ( [], [] )

        PasswordChange password ->
            { model | password = password } !: []

        PasswordRepeatChange passwordRepeat ->
            { model | passwordRepeat = passwordRepeat } !: []

        RequestSignup ->
            model
                !: [ Http.send SignupResponse <|
                        signupRequest model.serverUrl
                            { firstName = model.firstName
                            , lastName = model.lastName
                            , email = model.email
                            , password = model.password
                            }
                   ]

        SignupResponse res ->
            case decodeSignupResponse res of
                SignupSuccessResponse ->
                    model !> ([], [ send <| Globals.Types.Alert "signup success"])
                SignupErrorResponse ->
                    model !> ([], [ send <| Globals.Types.Alert "signup error"])


view : Model -> Globals.Types.Model -> Html Msg
view model globals =
    div []
        [ h1 [ style [ ( "margin-bottom", "1.2em" ) ] ] [ text "Sign up for a new flatr account" ]
        , Form.form [ onSubmit RequestSignup ]
            [ Form.group []
                [ Form.label [ for "firstName" ] [ text "First name:" ]
                , Input.text [ Input.id "firstName", onInput FirstNameChange ]
                ]
            , Form.group []
                [ Form.label [ for "lastName" ] [ text "Last name:" ]
                , Input.text [ Input.id "lastName", onInput LastNameChange ]
                ]
            , Form.group []
                [ Form.label [ for "email" ] [ text "E-Mail:" ]
                , Input.email [ Input.id "email", onInput EmailChange ]
                ]
            , Form.group []
                [ Form.label [ for "password" ] [ text "Password:" ]
                , Input.password [ Input.id "password", onInput PasswordChange ]
                ]
            , Form.group []
                [ Form.label [ for "passwordRepeat" ] [ text "Repeat password:" ]
                , Input.password [ Input.id "passwordRepeat", onInput PasswordRepeatChange ]
                ]
            , Form.group []
                [ Form.help []
                    [ text "Your password will never be stored in plaintext. "
                    , text "Please make sure that this website is visited "
                    , text "over a secure HTTPS connection!"
                    ]
                ]
            , div [ style [ ( "float", "clear" ) ] ]
                [ div [ style [ ( "float", "left" ) ] ] [ Button.button [ Button.primary ] [ text "Sign up" ] ]
                , div [ style [ ( "float", "right" ) ] ] [ Button.linkButton [ Button.secondary, Button.attrs [ href "#login" ] ] [ text "Login with an existing account" ] ]
                ]
            ]
        ]

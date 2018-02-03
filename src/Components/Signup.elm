module Components.Signup exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input exposing (onInput)
import Globals.Types
import Helpers.Authentication exposing (..)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Toast exposing (errorToast)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (for, href, id, style)
import Html.Events exposing (onSubmit)
import Http
import Task


type SignupState
    = SignupForm
    | SignupPending
    | SignupSuccessEmail
    | SignupSuccessNoEmail


type alias Model =
    { state : SignupState
    , email : String
    , firstName : String
    , lastName : String
    , password : String
    , passwordRepeat : String
    , invitationCode : Maybe String
    , serverInputDefault : String
    , serverInput : String
    , serverUrl : String
    }


initialModel : String -> Model
initialModel serverInput =
    { state = SignupForm
    , email = ""
    , firstName = ""
    , lastName = ""
    , password = ""
    , passwordRepeat = ""
    , invitationCode = Nothing
    , serverInputDefault = serverInput -- Gets passed in by JavaScript (GET Param)
    , serverInput = serverInput
    , serverUrl = serverInput
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
                    initialModel model.serverInputDefault !: []

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
                            , invitationCode = model.invitationCode
                            }
                   ]

        SignupResponse res ->
            case signupResponseDecode res of
                SignupSuccessResponse { email, emailVerified } ->
                    { model
                        | state =
                            case emailVerified of
                                True ->
                                    SignupSuccessNoEmail

                                False ->
                                    SignupSuccessEmail
                    }
                        !: []

                SignupErrorResponse err ->
                    case err.error of
                        NotInvitedError ->
                            model
                                !: [ errorToast "Not invited" <|
                                        "No invitation could be found for this email."
                                            ++ "<br />Please check your spelling or ask a registered user to invite you."
                                   ]

                        InvitationCodeInvalidError ->
                            model !: [ errorToast "Invitation Code invalid" err.message ]

                        UnknownSignupError _ ->
                            model !: [ errorToast "Unknown Error" err.message ]

                _ ->
                    -- TODO: Account for network errors seperately
                    model !: [ errorToast "Communication Error" "An error occured while communicating to the server." ]


view : Model -> Globals.Types.Model -> Html Msg
view model globals =
    case model.state of
        SignupForm ->
            signupFormView model globals True

        SignupPending ->
            signupFormView model globals False

        _ ->
            signupFormView model globals False


signupFormView : Model -> Globals.Types.Model -> Bool -> Html Msg
signupFormView model globals formEnable =
    let
        dInput =
            Input.disabled <| not formEnable

        dButton =
            Button.disabled <| not formEnable
    in
    div []
        [ h1 [ style [ ( "margin-bottom", "1.2em" ) ] ]
            [ if model.invitationCode == Nothing then
                text "Sign up for a new flatr account"
              else
                text "Sign up via invitation link"
            ]
        , Form.form [ id "signup-form", onSubmit RequestSignup ]
            ([ Form.group []
                [ Form.label [ for "firstName" ] [ text "First name:" ]
                , Input.text [ dInput, Input.id "firstName", onInput FirstNameChange ]
                ]
             , Form.group []
                [ Form.label [ for "lastName" ] [ text "Last name:" ]
                , Input.text [ dInput, Input.id "lastName", onInput LastNameChange ]
                ]
             ]
                ++ (if model.invitationCode == Nothing then
                        [ Form.group []
                            [ Form.label [ for "email" ] [ text "E-Mail:" ]
                            , Input.email [ dInput, Input.id "email", onInput EmailChange ]
                            ]
                        ]
                    else
                        []
                   )
                ++ [ Form.group []
                        [ Form.label [ for "password" ] [ text "Password:" ]
                        , Input.password [ dInput, Input.id "password", onInput PasswordChange ]
                        ]
                   , Form.group []
                        [ Form.label [ for "passwordRepeat" ] [ text "Repeat password:" ]
                        , Input.password [ dInput, Input.id "passwordRepeat", onInput PasswordRepeatChange ]
                        ]
                   , Form.group []
                        [ Form.help []
                            [ text "Your password will never be stored in plaintext. "
                            , text "Please make sure that this website is visited "
                            , text "over a secure HTTPS connection!"
                            ]
                        ]
                   , div [ style [ ( "float", "clear" ) ] ]
                        [ div [ style [ ( "float", "left" ) ] ] [ Button.button [ dButton, Button.primary ] [ text "Sign up" ] ]
                        , div [ style [ ( "float", "right" ) ] ] [ Button.linkButton [ dButton, Button.secondary, Button.attrs [ href "#login" ] ] [ text "Login with an existing account" ] ]
                        ]
                   ]
            )
        ]

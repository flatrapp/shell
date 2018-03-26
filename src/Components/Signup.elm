module Components.Signup exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input exposing (onInput, value)
import Bootstrap.Grid as Grid
import Components.ServerInput as ServerInput
import Globals.Types
import Helpers.Api.Authentication exposing (..)
import Helpers.Functions exposing (send)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Toast exposing (errorToast)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class, for, href, id, required, style)
import Html.Events exposing (onSubmit)
import Http
import Task
import Time exposing (Time)


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
    , serverInput : ServerInput.Model
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
    , serverInput = ServerInput.initialModel serverInput
    }


type Msg
    = AppInitialized
    | ViewState Bool
    | TimeTick Time
    | ServerInputMsg ServerInput.Msg
    | FirstNameChange String
    | LastNameChange String
    | EmailChange String
    | PasswordChange String
    | PasswordRepeatChange String
    | RequestSignup
    | SignupResponse (Result Http.Error SignupResponse)


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        AppInitialized ->
            model !: []

        ViewState state ->
            case state of
                False ->
                    initialModel (ServerInput.getPrefilledInput model.serverInput) !: []

                True ->
                    model !: []

        TimeTick time ->
            let
                ( newModel, cmd ) =
                    ServerInput.update (ServerInput.TimeTick time) model.serverInput globals
            in
            { model | serverInput = newModel } !: [ Cmd.map ServerInputMsg cmd ]

        ServerInputMsg serverInputMsg ->
            let
                ( newModel, cmd ) =
                    ServerInput.update serverInputMsg model.serverInput globals
            in
            { model | serverInput = newModel } !: [ Cmd.map ServerInputMsg cmd ]

        FirstNameChange firstName ->
            { model | firstName = firstName } !: []

        LastNameChange lastName ->
            { model | lastName = lastName } !: []

        EmailChange email ->
            { model | email = email } !: []

        PasswordChange password ->
            { model | password = password } !: []

        PasswordRepeatChange passwordRepeat ->
            { model | passwordRepeat = passwordRepeat } !: []

        RequestSignup ->
            case ServerInput.getUrl model.serverInput of
                Nothing ->
                    model !: [ errorToast "Inputs invalid" "The entered server could not be converted to a well-formed url." ]

                Just serverUrl ->
                    case model.password == model.passwordRepeat of
                        True ->
                            model
                                !: [ Http.send SignupResponse <|
                                        signupRequest serverUrl
                                            { firstName = model.firstName
                                            , lastName = model.lastName
                                            , email = model.email
                                            , password = model.password
                                            , invitationCode = model.invitationCode
                                            }
                                   ]

                        False ->
                            model !: [ errorToast "Inputs invalid" "The password and password repeat fields do not match" ]

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
                        EmailCollisionError ->
                            model
                                !: [ errorToast "User already exists" <|
                                        "A user with this email already exists."
                                            ++ "<br />Maybe you want to <a href=\"#login\">login</a> instead?"
                                   ]

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

        SignupSuccessEmail ->
            signupSuccessView True

        SignupSuccessNoEmail ->
            signupSuccessView False


requiredInput : Input.Option msg
requiredInput =
    Input.attrs [ required True ]


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
            ([ Html.map ServerInputMsg <| ServerInput.view model.serverInput formEnable
             , Form.group []
                [ Form.label [ for "firstName" ] [ text "First name:" ]
                , Input.text [ dInput, Input.id "firstName", onInput FirstNameChange, requiredInput ]
                ]
             , Form.group []
                [ Form.label [ for "lastName" ] [ text "Last name:" ]
                , Input.text [ dInput, Input.id "lastName", onInput LastNameChange, requiredInput ]
                ]
             ]
                ++ (if model.invitationCode == Nothing then
                        [ Form.group []
                            [ Form.label [ for "email" ] [ text "E-Mail:" ]
                            , Input.email [ dInput, Input.id "email", onInput EmailChange, requiredInput ]
                            ]
                        ]
                    else
                        []
                   )
                ++ [ Form.group []
                        [ Form.label [ for "password" ] [ text "Password:" ]
                        , Input.password [ dInput, Input.id "password", onInput PasswordChange, requiredInput ]
                        ]
                   , Form.group []
                        [ Form.label [ for "passwordRepeat" ] [ text "Repeat password:" ]
                        , Input.password
                            [ dInput
                            , Input.id "passwordRepeat"
                            , onInput PasswordRepeatChange
                            , requiredInput
                            , Input.attrs
                                [ class <|
                                    case model.password == model.passwordRepeat of
                                        True ->
                                            "is-valid"

                                        False ->
                                            "is-invalid"
                                ]
                            ]
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


signupSuccessView : Bool -> Html msg
signupSuccessView emailVerificationRequired =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ h1 [] [ text "Signup successful!" ]
                ]
            ]
        , case emailVerificationRequired of
            False ->
                Grid.row []
                    [ Grid.col []
                        [ p []
                            [ text "Your email has already been confirmed. You can now proceed to login!" ]
                        ]
                    ]

            True ->
                Grid.row []
                    [ Grid.col []
                        [ p [] [ text "Your email needs to be verified. We sent you an email. Please click on the link inside to confirm it. Also check your spam folder!" ] ]
                    ]
        , Grid.row []
            [ Grid.col []
                [ div [ style [ ( "float", "right" ) ] ] [ Button.linkButton [ Button.secondary, Button.attrs [ href "#login" ] ] [ text "Proceed to login" ] ]
                ]
            ]
        ]

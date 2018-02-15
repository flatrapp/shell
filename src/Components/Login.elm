module Components.Login exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input exposing (onInput, value)
import Exts.Html
import Globals.Types
import Helpers.Api.Authentication exposing (..)
import Helpers.Api.Server exposing (ServerInfoResponse(..), saveServerInput, serverInfoRequest, serverInfoResponseDecode)
import Helpers.Authentication exposing (saveAuthentication)
import Helpers.Functions exposing (..)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Toast exposing (errorToast)
import Helpers.UrlRegex exposing (checkUrlInput, urlRegexString)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (for, href, id, pattern, required, style)
import Html.Events exposing (onSubmit)
import Http
import Navigation
import Task
import Time exposing (Time, second)


serverInputCheckTime : Float
serverInputCheckTime =
    1 * second


type alias Model =
    { state : State
    , email : String
    , password : String
    , serverInputDefault : String
    , serverInput : String
    , serverUrl : Maybe String
    , serverInputCheckVal : String
    , serverInputLastChange : Time
    , serverInputCheckCount : Int
    , serverState : ServerState
    }


initialModel : String -> Model
initialModel serverInput =
    { state = LoginForm
    , email = ""
    , password = ""
    , serverInputDefault = serverInput
    , serverInput = serverInput
    , serverUrl = checkUrlInput serverInput
    , serverInputCheckVal = ""
    , serverInputLastChange = 0
    , serverInputCheckCount = 0
    , serverState = None
    }


type State
    = LoginForm
    | LoginPending


type ServerState
    = Pending
    | ConnectOk String String
    | ConnectErr
    | None


type Msg
    = AppInitialized
    | RequestAuthentication
    | EmailChange String
    | PasswordChange String
    | ServerInputChange String
    | AuthResponse (Result Http.Error AuthenticationResponse)
    | SaveServerInput String
    | ViewState Bool
    | ServerConnectResponse Int (Result Http.Error ServerInfoResponse)
    | TimeTick Time.Time


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
            if model.serverInput /= model.serverInputCheckVal && time > model.serverInputLastChange + serverInputCheckTime then
                case maybe2 ( model.serverUrl, globals.time ) of
                    Nothing ->
                        model !: []

                    Just ( serverUrl, time ) ->
                        { model
                            | serverInputCheckVal = model.serverInput
                            , serverInputCheckCount = model.serverInputCheckCount + 1
                            , serverState = Pending
                        }
                            !: [ Http.send (ServerConnectResponse <| model.serverInputCheckCount + 1) <|
                                    serverInfoRequest serverUrl time
                               ]
            else
                model !: []

        ServerConnectResponse checkCount res ->
            if checkCount >= model.serverInputCheckCount then
                case serverInfoResponseDecode res of
                    ServerInfoSuccessResponse info ->
                        { model | serverState = ConnectOk info.name info.version } !: []

                    _ ->
                        { model | serverState = ConnectErr } !: []
            else
                -- We've already sent anotehr request, ignore old ones
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
                newModel =
                    { model | serverUrl = checkUrlInput model.serverInput }
            in
            case newModel.serverUrl of
                Nothing ->
                    newModel !: [ errorToast "Inputs invalid" "The entered server could not be converted to a well-formed url." ]

                Just serverUrl ->
                    { newModel | state = LoginPending }
                        !: [ Http.send AuthResponse (authRequest serverUrl newModel.email newModel.password) ]

        AuthResponse res ->
            let
                ( cmd, globalsCmd ) =
                    handleAuthResponse globals
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
                time =
                    case globals.time of
                        Just time ->
                            time

                        Nothing ->
                            0

                serverUrl =
                    checkUrlInput serverInput

                serverState =
                    if serverUrl == Nothing then
                        None
                    else
                        Pending
            in
            { model
                | serverInput = serverInput
                , serverUrl = serverUrl
                , serverInputLastChange = time
                , serverState = serverState
                , serverInputCheckCount = model.serverInputCheckCount + 1
            }
                !: []


handleAuthResponse :
    Globals.Types.Model
    -> (Globals.Types.Authentication -> ( Cmd Msg, Cmd Globals.Types.Msg ))
    -> Result Http.Error AuthenticationResponse
    -> ( Cmd Msg, Cmd Globals.Types.Msg )
handleAuthResponse globals successCmdFn res =
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
                            toAuthentication authSuccess time

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


requiredInput : Input.Option msg
requiredInput =
    Input.attrs [ required True ]


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
                , Form.help [ style [ ( "float", "right" ) ] ]
                    [ text <|
                        case model.serverUrl of
                            Nothing ->
                                Exts.Html.nbsp

                            Just url ->
                                url
                    , text " - "
                    , text <|
                        case model.serverState of
                            Pending ->
                                "Pending"

                            ConnectErr ->
                                "Connect Error"

                            ConnectOk name version ->
                                "Connect Ok - " ++ name ++ " " ++ version

                            None ->
                                ""
                    ]
                , Input.text <| inputAttrs formEnable True "server" model.serverInput ServerInputChange
                ]
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

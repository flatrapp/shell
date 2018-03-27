module Components.ServerInput exposing (Model, Msg(..), getPrefilledInput, getUrl, initialModel, update, view)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Exts.Html
import Globals.Types
import Helpers.Api.Server exposing (ServerInfoResponse(..), saveServerInput, serverInfoRequest, serverInfoResponseDecode)
import Helpers.Functions exposing (..)
import Helpers.Operators exposing (..)
import Helpers.UrlRegex exposing (checkUrlInput)
import Html exposing (Html, text)
import Html.Attributes exposing (for, required, style)
import Http
import Time exposing (Time, second)


serverInputCheckTime : Float
serverInputCheckTime =
    1 * second


type alias Model =
    { serverState : ServerState
    , prefilledInput : String
    , input : String
    , url : Maybe String
    , checkVal : String
    , lastChange : Time
    , checkCount : Int
    }


initialModel : String -> Model
initialModel serverInput =
    { serverState = None
    , prefilledInput = serverInput
    , input = serverInput
    , url = checkUrlInput serverInput
    , checkVal = "" -- What does this do?
    , lastChange = 0
    , checkCount = 0
    }


getPrefilledInput : Model -> String
getPrefilledInput model =
    model.prefilledInput


getUrl : Model -> Maybe String
getUrl model =
    -- Generate the url from the input here to get the most up-to-date
    checkUrlInput model.input


type ServerState
    = None
    | Pending
    | ConnectOk String String
    | ConnectErr


type Msg
    = TimeTick Time
    | InputChange String
    | SaveInput
    | ConnectResponse Int (Result Http.Error ServerInfoResponse)


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg)
update msg model globals =
    case msg of
        TimeTick time ->
            if model.input /= model.checkVal && time > model.lastChange + serverInputCheckTime then
                case maybe2 ( model.url, globals.time ) of
                    Nothing ->
                        model !: []

                    Just ( url, time ) ->
                        { model
                            | checkVal = model.input
                            , checkCount = model.checkCount + 1
                            , serverState = Pending
                        }
                            !: [ Http.send (ConnectResponse <| model.checkCount + 1) <|
                                    serverInfoRequest url time
                              ]
            else
                model !: []

        ConnectResponse checkCount res ->
            if checkCount >= model.checkCount then
                case serverInfoResponseDecode res of
                    ServerInfoSuccessResponse info ->
                        { model | serverState = ConnectOk info.name info.version } !: []

                    _ ->
                        { model | serverState = ConnectErr } !: []
            else
                -- We've already sent another more recent request, ignore old one
                model !: []

        SaveInput ->
            { model | prefilledInput = model.input } !> ([], [ send <| Globals.Types.SaveServerInput model.input ])

        InputChange serverInput ->
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
                | input = serverInput
                , url = serverUrl
                , lastChange = time
                , serverState = serverState
                , checkCount = model.checkCount + 1
            }
                !: []


requiredInput : Input.Option msg
requiredInput =
    Input.attrs [ required True ]


inputAttrs : Bool -> Bool -> String -> String -> (String -> Msg) -> List (Input.Option Msg)
inputAttrs enabled requiredVal id val msg =
    [ Input.disabled <| not enabled
    , Input.attrs [ required requiredVal ]
    , Input.id id
    , Input.value val
    , Input.onInput msg
    ]


view : Model -> Bool -> Html Msg
view model formEnable =
    Form.group []
        [ Form.label [ for "server" ] [ text "Server (There's one per flat):" ]
        , Form.help [ style [ ( "float", "right" ) ] ]
            [ text <|
                case model.url of
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
        , Input.text <| inputAttrs formEnable True "server" model.input InputChange
        ]

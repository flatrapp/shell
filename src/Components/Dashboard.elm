module Components.Dashboard exposing (..)

import Bootstrap.Grid as Grid
import Globals.Types
import Guards exposing (..)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.User as User exposing (decodeCurrentUserResponse)
import Html exposing (Html, br, hr, text)
import Http
import Task
import Time
import Time.DateTime as DateTime exposing (DateTime)


greeting : Int -> Time.Time -> String
greeting timezoneOffset time =
    let
        hour =
            DateTime.hour <| DateTime.addMinutes timezoneOffset <| DateTime.fromTimestamp time
    in
    (hour >= 21 || hour < 5)
        => "Good Night"
        |= (hour >= 5 && hour < 10)
        => "Good Morning"
        |= (hour >= 17 && hour < 21)
        => "Good Evening"
        |= "Hello"


updateInterval : Time.Time
updateInterval =
    10 * Time.second


type alias Model =
    { viewState : Bool
    , lastUpdate : Time.Time
    , currentUser : Maybe User.UserInfo
    }


initialModel : Model
initialModel =
    { viewState = False
    , lastUpdate = 0
    , currentUser = Nothing
    }


type Msg
    = ViewState Bool
    | TimeTick Time.Time
    | ShowView
    | HideView
    | UpdateData
    | CurrentUserResponse (Result Http.Error User.CurrentUserResponse)


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        ViewState state ->
            if model.viewState == False && state == True then
                update ShowView { model | viewState = state } globals
            else if model.viewState == True && state == False then
                update HideView { model | viewState = state } globals
            else
                model !: []

        ShowView ->
            update UpdateData model globals

        HideView ->
            -- Clear all data that could change while the component is hidden
            { model | currentUser = Nothing } !: []

        TimeTick time ->
            if model.viewState && (model.lastUpdate + updateInterval) < time then
                update UpdateData model globals
            else
                model !: []

        UpdateData ->
            case globals.auth of
                Just auth ->
                    case globals.time of
                        Just time ->
                            { model | lastUpdate = time }
                                !: [ Http.send CurrentUserResponse <|
                                        User.currentUserRequest globals.apiBaseUrl auth
                                   ]

                        Nothing ->
                            model !: []

                Nothing ->
                    model !: []

        CurrentUserResponse result ->
            case decodeCurrentUserResponse result of
                User.CurrentUserSuccessResponse user ->
                    { model | currentUser = Just user } !: []

                User.CurrentUserErrorResponse { error, message } ->
                    case error of
                        User.CurrentUserUnknownError ->
                            model !> ( [], [ send <| Globals.Types.Alert <| "Unknown Error: " ++ message ] )

                _ ->
                    model !> ( [], [ send <| Globals.Types.Alert <| "Unknown Error, you might want to check your network connection." ] )


view : Model -> Globals.Types.Model -> ( Html msg, Html msg )
view model globals =
    case model.currentUser of
        Nothing ->
            ( loadingScreen, text "" )

        Just user ->
            case globals.time of
                Nothing ->
                    ( loadingScreen, text "" )

                Just time ->
                    content model globals user time


loadingScreen : Html msg
loadingScreen =
    Html.h1 [] [ text "Loading..." ]


content : Model -> Globals.Types.Model -> User.UserInfo -> Time.Time -> ( Html msg, Html msg )
content model globals user time =
    ( Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ text <| greeting globals.timezoneOffset time ++ ", " ++ user.firstName ++ " " ++ user.lastName ++ "!" ]
            ]
        ]
    , text "Fancy footer message from the Dashboard Component"
    )

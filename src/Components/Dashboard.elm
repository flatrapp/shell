module Components.Dashboard exposing (..)

import Bootstrap.Badge as Badge
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Dict exposing (Dict)
import Globals.Types
import Guards exposing (..)
import Helpers.Functions exposing (..)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Task as Task exposing (..)
import Helpers.Toast exposing (errorToast)
import Helpers.User as User exposing (UserInfo, currentUserResponseDecode, listUsersResponseDecode)
import Html exposing (Html, br, div, h5, hr, p, small, span, text)
import Html.Attributes exposing (class, style)
import Http
import Task as ElmTask
import Time exposing (Time)
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
    , currentUser : Maybe UserInfo
    , users : Maybe (Dict Int UserInfo)
    , tasks : Maybe (List Task.Task)
    }


initialModel : Model
initialModel =
    { viewState = False
    , lastUpdate = 0
    , currentUser = Nothing
    , users = Nothing
    , tasks = Nothing
    }


type Msg
    = ViewState Bool
    | TimeTick Time.Time
    | ShowView
    | HideView
    | UpdateData
    | CurrentUserResponse (Result Http.Error User.CurrentUserResponse)
    | ListUsersResponse (Result Http.Error User.ListUsersResponse)
    | ListTasksResponse (Result Http.Error Task.ListTasksResponse)


send : msg -> Cmd msg
send msg =
    ElmTask.succeed msg
        |> ElmTask.perform identity


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
                                        User.currentUserRequest auth
                                   , Http.send ListUsersResponse <|
                                        User.listUsersRequest auth
                                   , Http.send ListTasksResponse <|
                                        Task.listTasksRequest auth
                                   ]

                        Nothing ->
                            model !: []

                Nothing ->
                    model !: []

        CurrentUserResponse result ->
            case currentUserResponseDecode result of
                User.CurrentUserSuccessResponse user ->
                    { model | currentUser = Just user } !: []

                User.CurrentUserErrorResponse { error, message } ->
                    case error of
                        User.CurrentUserUnauthorizedError ->
                            model
                                !> ( [ errorToast "Session Expired" "You have been logged out because the session got invalid." ]
                                   , [ send <| Globals.Types.Logout ]
                                   )

                        User.CurrentUserUnknownError err ->
                            model !: [ errorToast "Unknown Error while retrieving user info" message ]

                _ ->
                    -- TODO: Check network errors seperately
                    model !: [ errorToast "Connection Error" "There was an error while communicating with the server." ]

        ListUsersResponse result ->
            case listUsersResponseDecode result of
                User.ListUsersSuccessResponse users ->
                    { model | users = Just users } !: []

                _ ->
                    -- TODO: Handle errors here
                    model !: [ errorToast "Unknown Error" "An unknown error occured while retrieving the list of users" ]

        ListTasksResponse result ->
            case listTasksResponseDecode result of
                Task.ListTasksSuccessResponse tasks ->
                    { model | tasks = Just tasks } !: []

                _ ->
                    -- TODO: Handle errors here
                    model !: [ errorToast "Unknown Error" "An unknown error occured while retrieving the list of tasks" ]


view : Model -> Globals.Types.Model -> ( Html msg, Html msg )
view model globals =
    case maybe4 ( model.currentUser, model.tasks, model.users, globals.time ) of
        Nothing ->
            ( loadingScreen, text "" )

        Just ( currentUser, tasks, users, time ) ->
            case Task.tasksMapUser users tasks of
                Just tasksUser ->
                    content model globals tasksUser currentUser time

                Nothing ->
                    ( text "Error: A user specified by a task could not be found!", text "" )


loadingScreen : Html msg
loadingScreen =
    Html.h1 [] [ text "Loading..." ]


content : Model -> Globals.Types.Model -> List Task.TaskUser -> UserInfo -> Time.Time -> ( Html msg, Html msg )
content model globals tasks currentUser time =
    ( Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ Html.h1 [] [ text <| greeting globals.timezoneOffset time ++ ", " ++ currentUser.firstName ++ " " ++ currentUser.lastName ++ "!" ] ]
            ]
        , hr [] []
        , br [] []
        , Grid.row []
            [ Grid.col []
                [ Card.config []
                    -- Card.info ]
                    |> Card.headerH5 [] [ text "Current tasks" ]
                    |> Card.listGroup (renderTasks True tasks time globals.timezoneOffset)
                    |> Card.view
                ]
            ]
        , br [] []
        , br [] []
        , Grid.row []
            [ Grid.col []
                [ Card.config []
                    -- Card.info ]
                    |> Card.headerH5 [] [ text "Upcoming tasks" ]
                    |> Card.listGroup (renderTasks False tasks time globals.timezoneOffset)
                    |> Card.view
                ]
            ]
        ]
    , text "Fancy footer message from the Dashboard Component"
    )


renderTasks : Bool -> List Task.TaskUser -> Time -> Int -> List (ListGroup.Item msg)
renderTasks displayCurrentTurn tasks time timezoneOffset =
    List.filterMap
        (\task ->
            if displayCurrentTurn then
                case task.currentTurn of
                    Nothing ->
                        Nothing

                    Just turn ->
                        Just <| renderCurrentTask task turn time timezoneOffset
            else
                case List.head task.upcomingTurns of
                    Nothing ->
                        Nothing

                    Just turn ->
                        Just <| renderUpcomingTask task turn time timezoneOffset
        )
        tasks


taskRemainingTime : DateTime -> Int -> Time -> Int -> String
taskRemainingTime start completionTime time timezoneOffset =
    let
        end =
            DateTime.addHours completionTime start

        current =
            DateTime.fromTimestamp time

        delta =
            DateTime.delta end current

        daysString =
            if delta.days == 1 then
                toString delta.days ++ " day "
            else if delta.days > 1 then
                toString delta.days ++ " days "
            else
                ""

        hoursString =
            if delta.hours == 1 then
                toString (delta.hours % 24) ++ " hours left"
            else
                toString (delta.hours % 24) ++ " hours left"
    in
    daysString ++ hoursString


renderCurrentTask : Task.TaskUser -> Task.TurnUser -> Time -> Int -> ListGroup.Item msg
renderCurrentTask task turn time timezoneOffset =
    ListGroup.li [ ListGroup.attrs [ class "flex-column align-items-start" ] ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ span [ style [ ( "margin-right", "20px" ) ] ] [ text task.title ]
                , Badge.badge [] [ text <| turn.user.firstName ++ " " ++ turn.user.lastName ]
                ]
            , h5 [ class "mb-1" ]
                [ Badge.badgeWarning []
                    [ text <|
                        taskRemainingTime turn.start task.completionTime time timezoneOffset
                    ]
                ]
            , p [ class "mb-1" ] [ text task.description ]
            ]
        ]


renderUpcomingTask : Task.TaskUser -> Task.TurnUser -> Time -> Int -> ListGroup.Item msg
renderUpcomingTask task turn time timezoneOffset =
    ListGroup.li [ ListGroup.attrs [ class "flex-column align-items-start" ] ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ span [ style [ ( "margin-right", "20px" ) ] ] [ text task.title ] ]
            ]
        ]

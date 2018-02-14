module Components.Dashboard exposing (..)

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Dict exposing (Dict)
import Globals.Types
import Guards exposing (..)
import Helpers.Functions exposing (..)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Api.Task as Task exposing (..)
import Helpers.Toast exposing (errorToast)
import Helpers.Api.User as User exposing (UserInfo, currentUserResponseDecode, listUsersResponseDecode)
import Html exposing (Html, br, div, h5, hr, p, small, span, text)
import Html.Attributes exposing (class, style)
import Http
import Task as ElmTask
import Time exposing (Time)
import Time.DateTime as DateTime exposing (DateTime)


updateInterval : Time.Time
updateInterval =
    1.5 * Time.minute


type alias Model =
    { viewState : Bool
    , lastUpdate : Time.Time
    , currentUser : Maybe UserInfo
    , users : Maybe (Dict Int UserInfo)
    , tasks : Maybe (Dict Int Task.Task)
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
    | FinishTurn Int
    | CurrentUserResponse (Result Http.Error User.CurrentUserResponse)
    | ListUsersResponse (Result Http.Error User.ListUsersResponse)
    | ListTasksResponse (Result Http.Error Task.ListTasksResponse)
    | FinishTurnResponse (Result Http.Error Task.FinishTurnResponse)


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
            -- Some data could have changed, load immediately
            update UpdateData model globals

        HideView ->
            model !: []

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

        FinishTurn id ->
            case globals.auth of
                Nothing ->
                    model !: []

                Just auth ->
                    model
                        !: [ Http.send FinishTurnResponse <| finishTurnRequest auth id ]

        FinishTurnResponse res ->
            case finishTurnResponseDecode res of
                FinishTurnSuccessResponse ->
                    case globals.auth of
                        Just auth ->
                            model !: [ Http.send ListTasksResponse <| Task.listTasksRequest auth ]

                        Nothing ->
                            model !: []

                FinishTurnErrorResponse _ ->
                    -- TODO do finegrained error handling
                    model !: [ errorToast "Error" "Unkown error while finishing turn." ]

                FinishTurnInvalidResponse ->
                    model !: [ errorToast "Error" "Unkown error while finishing turn." ]

                FinishTurnHttpError _ ->
                    -- TODO maybe to finegrained error handling
                    model !: [ errorToast "Connection Error" "There was an error while communicating with the server." ]

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

                err ->
                    -- TODO: Handle errors here
                    model !: [ errorToast "Unknown Error" "An unknown error occured while retrieving the list of tasks" ]


view : Model -> Globals.Types.Model -> ( Html Msg, Html msg )
view model globals =
    case maybe4 ( model.currentUser, model.tasks, model.users, globals.time ) of
        Nothing ->
            ( loadingScreen, text "" )

        Just ( currentUser, tasks, users, time ) ->
            case Task.tasksMapUser users <| Dict.values tasks of
                Just tasksUser ->
                    content model globals tasksUser currentUser time

                Nothing ->
                    ( text "Error: A user specified by a task could not be found!", text "" )


loadingScreen : Html msg
loadingScreen =
    Html.h1 [] [ text "Loading..." ]


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


content : Model -> Globals.Types.Model -> List Task.TaskUser -> UserInfo -> Time.Time -> ( Html Msg, Html msg )
content model globals tasks currentUser time =
    ( Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ Html.h1 [] [ text <| greeting globals.timezoneOffset time ++ ", " ++ currentUser.firstName ++ "!" ] ]
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


renderTasks : Bool -> List Task.TaskUser -> Time -> Int -> List (ListGroup.Item Msg)
renderTasks displayCurrentTurn tasks time timezoneOffset =
    let
        filteredTasks =
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

        sortedTasksHtml =
            List.sortBy Tuple.second filteredTasks
                |> List.map Tuple.first
    in
    if List.length filteredTasks > 0 then
        sortedTasksHtml
    else if displayCurrentTurn then
        [ noCurrentTasks ]
    else
        [ noUpcomingTasks ]


taskRemainingTime : DateTime -> Int -> Time -> Int -> ( String, Int )
taskRemainingTime start completionTime time timezoneOffset =
    let
        end =
            DateTime.addHours completionTime start

        current =
            DateTime.fromTimestamp time

        delta =
            DateTime.delta end current

        days =
            delta.hours // 24

        hours =
            delta.hours % 24

        daysString =
            if days == 1 then
                toString days ++ " day "
            else if days > 1 then
                toString days ++ " days "
            else
                ""

        hoursString =
            if hours == 1 then
                toString hours ++ " hour left"
            else if delta.hours < 0 then
                "overdue by " ++ toString (delta.hours * -1) ++ " hours"
            else
                toString hours ++ " hours left"
    in
    ( daysString ++ hoursString, delta.minutes )


noCurrentTasks : ListGroup.Item msg
noCurrentTasks =
    ListGroup.li [ ListGroup.attrs [ class "flex-column align-items-start" ] ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ h5 [ class "mb-1" ] [ span [ style [ ( "margin-right", "20px" ) ] ] [ text "All done!" ] ] ]
        , p [ class "mb-1" ] [ text "No current tasks. Enjoy a well organized flat!" ]
        ]


noUpcomingTasks : ListGroup.Item msg
noUpcomingTasks =
    ListGroup.li [ ListGroup.attrs [ class "flex-column align-items-start" ] ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ h5 [ class "mb-1" ] [ span [ style [ ( "margin-right", "20px" ) ] ] [ text "Did you do the setup?" ] ] ]
        , p [ class "mb-1" ] [ text "No upcoming tasks... You could add some!" ]
        ]


renderCurrentTask : Task.TaskUser -> Task.TurnUser -> Time -> Int -> ( ListGroup.Item Msg, Int )
renderCurrentTask task turn time timezoneOffset =
    let
        ( remainingText, remainingTime ) =
            taskRemainingTime turn.start task.completionTime time timezoneOffset
    in
    ( ListGroup.li [ ListGroup.attrs [ class "flex-column align-items-start" ] ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ span [ style [ ( "margin-right", "20px" ) ] ] [ text task.title ]
                , Badge.badge [] [ text <| turn.user.firstName ++ " " ++ turn.user.lastName ]
                ]
            , small [] [ text remainingText ]
            ]
        , div [ class "d-flex w-100 justify-content-between" ]
            [ p [ class "mb-1" ] [ text task.description ]
            , Button.button
                [ Button.small, Button.success, Button.attrs [ class "ml-1" ], Button.onClick <| FinishTurn task.id ]
                [ text "Done" ]
            ]
        ]
    , remainingTime
    )


renderUpcomingTask : Task.TaskUser -> Task.TurnUser -> Time -> Int -> ( ListGroup.Item msg, Int )
renderUpcomingTask task turn time timezoneOffset =
    ( ListGroup.li [ ListGroup.attrs [ class "flex-column align-items-start" ] ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ span [ style [ ( "margin-right", "20px" ) ] ] [ text task.title ] ]
            ]
        ]
    , 0
    )

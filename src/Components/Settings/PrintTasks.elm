port module Components.Settings.PrintTasks exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Dict exposing (Dict)
import Exts.Html
import Globals.Types
import Helpers.Api.Task as Task exposing (..)
import Helpers.Api.User as User exposing (..)
import Helpers.Functions exposing (..)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Toast exposing (errorToast, successToast)
import Html exposing (Html, a, br, div, h2, h5, hr, i, li, nav, small, text, ul)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onSubmit)
import Http
import Set exposing (Set)
import Time exposing (Time)
import Time.DateTime as DateTime


port printWebsite : () -> Cmd msg


type alias Model =
    { time : Maybe Time
    , tasks : Maybe (Dict Int Task)
    , users : Maybe (Dict Int UserInfo)
    }


initialModel : Model
initialModel =
    { time = Nothing
    , tasks = Nothing
    , users = Nothing
    }


type Msg
    = ViewState Bool
    | UpdateData
    | TimeTick Time
    | ListTasksResponse (Result Http.Error Task.ListTasksResponse)
    | ListUsersResponse (Result Http.Error User.ListUsersResponse)


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        ViewState state ->
            case state of
                True ->
                    update UpdateData model globals

                False ->
                    model !: []

        TimeTick time ->
            { model | time = Just time } !: []

        UpdateData ->
            case globals.auth of
                Nothing ->
                    model !: []

                Just auth ->
                    model
                        !: [ Http.send ListTasksResponse <| listTasksRequest auth
                           , Http.send ListUsersResponse <| listUsersRequest auth
                           ]

        ListTasksResponse res ->
            case listTasksResponseDecode res of
                ListTasksSuccessResponse tasks ->
                    let
                        printCmd =
                            case model.users of
                                Nothing ->
                                    Cmd.none

                                Just _ ->
                                    printWebsite ()
                    in
                    { model | tasks = Just tasks } !: [ printCmd ]

                _ ->
                    -- TODO: Handle errors!
                    model !: []

        ListUsersResponse res ->
            case listUsersResponseDecode res of
                ListUsersSuccessResponse users ->
                    let
                        printCmd =
                            case model.tasks of
                                Nothing ->
                                    Cmd.none

                                Just _ ->
                                    printWebsite ()
                    in
                    { model | users = Just users } !: [ printCmd ]

                _ ->
                    -- TODO: Handle errors!
                    model !: []


view : Model -> Globals.Types.Model -> ( Html Msg, Html Msg, Bool )
view model globals =
    ( Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ h2 [] [ text "Tasks" ]
                , hr [] []
                ]
            ]
        , Grid.row [] [ Grid.col [] [ br [] [] ] ]
        , Grid.row []
            [ Grid.col [] [ tasksList model ] ]
        ]
    , text <|
        case model.time of
            Nothing ->
                ""

            Just time ->
                "Generated at " ++ (time |> DateTime.fromTimestamp |> DateTime.toISO8601)
    , False
    )


tasksList : Model -> Html Msg
tasksList model =
    case maybe2 ( model.tasks, model.users ) of
        Nothing ->
            ListGroup.ul
                [ ListGroup.li
                    [ ListGroup.attrs [ class "justify-content-between" ] ]
                    [ text "Loading users & tasks..." ]
                ]

        Just ( tasks, users ) ->
            Dict.values tasks
                |> List.map
                    (\task ->
                        tasksListEntry task users
                    )
                |> ListGroup.ul


tasksListEntry : Task -> Dict Int UserInfo -> ListGroup.Item Msg
tasksListEntry task users =
    ListGroup.li
        []
        [ div [ style [ ( "width", "100%" ) ] ]
            [ div [] [ h5 [] [ text task.title ] ]
            , div [] [ i [] [ text task.description ], text Exts.Html.nbsp ]
            ]
        ]

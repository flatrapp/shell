module Components.Settings.Tasks exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Dict exposing (Dict)
import Exts.Html
import Globals.Types
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Task as Task exposing (..)
import Helpers.Toast exposing (errorToast)
import Helpers.User as User exposing (..)
import Html exposing (Html, a, br, div, h2, h5, hr, i, li, nav, small, text, ul)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onSubmit)
import Http
import Set exposing (Set)


type alias Model =
    { tasks : Maybe (Dict Int Task)
    , users : Maybe (Dict Int UserInfo)
    , createTask : Bool
    , editTask : Maybe Int
    , editTaskTitle : String
    , editTaskDescription : String
    , editTaskFrequency : String
    , editTaskCompletionTime : String
    , editTaskUsers : Set Int
    }


initialModel : Model
initialModel =
    { tasks = Nothing
    , users = Nothing
    , createTask = False
    , editTask = Nothing
    , editTaskTitle = ""
    , editTaskDescription = ""
    , editTaskFrequency = ""
    , editTaskCompletionTime = ""
    , editTaskUsers = Set.empty
    }


type Msg
    = ViewState Bool
    | UpdateData
    | ListTasksResponse (Result Http.Error Task.ListTasksResponse)
    | ListUsersResponse (Result Http.Error User.ListUsersResponse)
    | EditTask Int
    | StopEdit
    | NewTask
    | EditTaskChangeUser Int Bool
    | EditTaskChangeTitle String
    | EditTaskChangeDescription String
    | EditTaskChangeFrequency String
    | EditTaskChangeCompletionTime String
    | UpdateTask Int
    | UpdateTaskResponse (Result Http.Error Task.UpdateTaskResponse)
    | CreateTask
    | CreateTaskResponse (Result Http.Error Task.CreateTaskResponse)


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        ViewState state ->
            case state of
                True ->
                    update UpdateData model globals

                False ->
                    model !: []

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
                    { model | tasks = Just tasks } !: []

                _ ->
                    -- TODO: Handle errors!
                    model !: []

        ListUsersResponse res ->
            case listUsersResponseDecode res of
                ListUsersSuccessResponse users ->
                    { model | users = Just users } !: []

                _ ->
                    -- TODO: Handle errors!
                    model !: []

        EditTask id ->
            case model.tasks of
                Nothing ->
                    model !: []

                Just tasks ->
                    case Dict.get id tasks of
                        Nothing ->
                            model !: []

                        Just task ->
                            setEditTask task model !: []

        NewTask ->
            let
                cleared =
                    clearEditTask model
            in
            { cleared | createTask = True } !: []

        StopEdit ->
            clearEditTask model !: []

        EditTaskChangeUser id state ->
            if state == True then
                { model | editTaskUsers = Set.insert id model.editTaskUsers } !: []
            else
                { model | editTaskUsers = Set.remove id model.editTaskUsers } !: []

        EditTaskChangeTitle title ->
            { model | editTaskTitle = title } !: []

        EditTaskChangeDescription description ->
            { model | editTaskDescription = description } !: []

        EditTaskChangeFrequency frequency ->
            { model | editTaskFrequency = frequency } !: []

        EditTaskChangeCompletionTime completionTime ->
            { model | editTaskCompletionTime = completionTime } !: []

        UpdateTask id ->
            let
                requestTask =
                    { title = model.editTaskTitle
                    , description = model.editTaskDescription
                    , users = model.editTaskUsers

                    -- TODO: Handle input errors correctly
                    , frequency = Result.withDefault -1 <| String.toInt model.editTaskFrequency
                    , completionTime = Result.withDefault -1 <| String.toInt model.editTaskCompletionTime
                    }
            in
            case globals.auth of
                Nothing ->
                    model !: []

                Just auth ->
                    model !: [ Http.send UpdateTaskResponse <| updateTaskRequest auth id requestTask ]

        UpdateTaskResponse res ->
            case updateTaskResponseDecode res of
                UpdateTaskSuccessResponse task ->
                    case model.tasks of
                        Nothing ->
                            -- This shouldn't happen at this stage, no error handling
                            model !: []

                        Just tasks ->
                            -- TODO: Create infotoast
                            { model | tasks = Just <| Dict.insert task.id task tasks }
                                !: [ errorToast "Success" "Task updated successfully" ]

                _ ->
                    -- TODO: Handle errors!
                    model !: [ errorToast "Update error" "An error occured while updating" ]

        CreateTask ->
            let
                requestTask =
                    { title = model.editTaskTitle
                    , description = model.editTaskDescription
                    , users = model.editTaskUsers

                    -- TODO: Handle input errors correctly
                    , frequency = Result.withDefault -1 <| String.toInt model.editTaskFrequency
                    , completionTime = Result.withDefault -1 <| String.toInt model.editTaskCompletionTime
                    }
            in
            case globals.auth of
                Nothing ->
                    model !: []

                Just auth ->
                    model !: [ Http.send CreateTaskResponse <| createTaskRequest auth requestTask ]

        CreateTaskResponse res ->
            case createTaskResponseDecode res of
                CreateTaskSuccessResponse task ->
                    case model.tasks of
                        Nothing ->
                            -- This shouldn't happen at this stage, no error handling
                            model !: []

                        Just tasks ->
                            -- TODO: Create infotoast
                            { model | tasks = Just <| Dict.insert task.id task tasks }
                                !: [ errorToast "Success" "Task created successfully" ]

                _ ->
                    -- TODO: Handle errors!
                    model !: [ errorToast "Update error" "An error occured while updating" ]


clearEditTask : Model -> Model
clearEditTask model =
    { model
        | editTask = Nothing
        , createTask = False
        , editTaskTitle = ""
        , editTaskDescription = ""
        , editTaskFrequency = ""
        , editTaskCompletionTime = ""
        , editTaskUsers = Set.empty
    }


setEditTask : Task -> Model -> Model
setEditTask task model =
    { model
        | editTask = Just task.id
        , createTask = False
        , editTaskTitle = task.title
        , editTaskDescription = task.description
        , editTaskFrequency = toString task.frequency
        , editTaskCompletionTime = toString task.completionTime
        , editTaskUsers = task.users
    }


view : Model -> Globals.Types.Model -> Html Msg
view model globals =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ h2 [] [ text "Tasks" ]
                , hr [] []
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ div [ style [ ( "float", "right" ) ] ]
                    [ Button.button
                        [ Button.small, Button.success, Button.attrs [ class "ml-1" ], Button.onClick NewTask ]
                        [ text "New Task" ]
                    ]
                ]
            ]
        , Grid.row [] [ Grid.col [] [ br [] [] ] ]
        , Grid.row []
            [ Grid.col [] [ tasksList model model.tasks model.editTask ] ]
        ]


tasksList : Model -> Maybe (Dict Int Task) -> Maybe Int -> Html Msg
tasksList model maybeTasks editTask =
    case maybeTasks of
        Nothing ->
            ListGroup.ul
                [ ListGroup.li
                    [ ListGroup.attrs [ class "justify-content-between" ] ]
                    [ text "Loading tasks..." ]
                ]

        Just tasks ->
            case model.users of
                Nothing ->
                    ListGroup.ul
                        [ ListGroup.li
                            [ ListGroup.attrs [ class "justify-content-between" ] ]
                            [ text "Loading users..." ]
                        ]

                Just users ->
                    Dict.values tasks
                        |> List.map
                            (\task ->
                                let
                                    showFn =
                                        case editTask of
                                            Just id ->
                                                if task.id == id then
                                                    tasksListEditEntry <| Just task
                                                else
                                                    tasksListEntry task

                                            Nothing ->
                                                tasksListEntry task
                                in
                                showFn model users
                            )
                        |> (\tasks ->
                                if model.createTask then
                                    [ tasksListEditEntry Nothing model users ] ++ tasks
                                else
                                    tasks
                           )
                        |> ListGroup.ul


tasksListEditEntry : Maybe Task -> Model -> Dict Int UserInfo -> ListGroup.Item Msg
tasksListEditEntry maybeTask model users =
    let
        ( newTask, id ) =
            case maybeTask of
                Nothing ->
                    ( True, -1 )

                Just task ->
                    ( False, task.id )
    in
    ListGroup.li [] <|
        [ Form.form
            [ onSubmit <|
                if newTask then
                    CreateTask
                else
                    UpdateTask id
            , style [ ( "width", "100%" ) ]
            ]
            [ Grid.container
                []
                [ Grid.row []
                    [ Grid.col []
                        [ h5 []
                            [ Input.text
                                [ Input.placeholder "Title"
                                , Input.value model.editTaskTitle
                                , Input.onInput EditTaskChangeTitle
                                ]
                            ]
                        ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ Input.text
                            [ Input.placeholder "Description"
                            , Input.value model.editTaskDescription
                            , Input.attrs [ style [ ( "font-style", "italic" ) ] ]
                            , Input.onInput EditTaskChangeDescription
                            ]
                        ]
                    ]
                , Grid.row [] [ Grid.col [] [ br [] [] ] ]
                , Grid.row []
                    [ Grid.col []
                        [ InputGroup.config
                            (InputGroup.number
                                [ Input.value model.editTaskFrequency
                                , Input.placeholder "3.5"
                                , Input.onInput EditTaskChangeFrequency
                                ]
                            )
                            |> InputGroup.predecessors
                                [ InputGroup.span [] [ text "Frequency: " ] ]
                            |> InputGroup.successors
                                [ InputGroup.span [] [ text "times / week" ] ]
                            |> InputGroup.view
                        ]
                    , Grid.col []
                        [ InputGroup.config
                            (InputGroup.number
                                [ Input.value model.editTaskCompletionTime
                                , Input.placeholder "36"
                                , Input.onInput EditTaskChangeCompletionTime
                                ]
                            )
                            |> InputGroup.predecessors
                                [ InputGroup.span [] [ text "Completion Time: " ] ]
                            |> InputGroup.successors
                                [ InputGroup.span [] [ text "h" ] ]
                            |> InputGroup.view
                        ]
                    ]
                , Grid.row [] [ Grid.col [] [ br [] [] ] ]
                , Grid.row []
                    [ Grid.col []
                        [ text "Users:"
                        , tasksListEditUsers users model.editTaskUsers
                        ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ div [ style [ ( "float", "right" ), ( "margin-top", "10px" ) ] ]
                            [ Button.button
                                [ Button.small, Button.warning, Button.attrs [ class "ml-1" ], Button.onClick StopEdit ]
                                [ text "Discard changes" ]
                            , Button.button
                                [ Button.small, Button.success, Button.attrs [ class "ml-1" ] ]
                                [ text <|
                                    if newTask then
                                        "Create Task"
                                    else
                                        "Save"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


tasksListEditUsers : Dict Int UserInfo -> Set Int -> Html Msg
tasksListEditUsers users taskUsers =
    let
        checkboxes =
            users
                |> Dict.values
                |> List.map
                    (\user ->
                        Checkbox.checkbox
                            [ Checkbox.inline
                            , Checkbox.checked <| Set.member user.id taskUsers
                            , Checkbox.onCheck <| EditTaskChangeUser user.id
                            ]
                        <|
                            user.firstName
                                ++ " "
                                ++ user.lastName
                    )
    in
    Grid.container [] [ Grid.row [] [ Grid.col [] checkboxes ] ]


tasksListEntry : Task -> Model -> Dict Int UserInfo -> ListGroup.Item Msg
tasksListEntry task _ users =
    ListGroup.li
        []
        [ div [ style [ ( "width", "100%" ) ] ]
            [ div [] [ h5 [] [ text task.title ] ]
            , div [] [ i [] [ text task.description ], text Exts.Html.nbsp ]
            , div [ style [ ( "float", "right" ) ] ]
                [ Button.button
                    [ Button.small, Button.warning, Button.attrs [ class "ml-1" ], Button.onClick <| EditTask task.id ]
                    [ text "Edit" ]
                ]
            ]
        ]

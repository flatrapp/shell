module Components.Settings exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Components.Settings.PrintTasks as PrintTasks
import Components.Settings.Tasks as Tasks
import Components.Settings.Users as Users
import Globals.Types
import Helpers.Functions exposing (send)
import Helpers.Operators exposing ((!:), (!>))
import Html exposing (Html, a, div, li, nav, text, ul)
import Html.Attributes exposing (class, href)
import Pages exposing (SettingsSubPage(..))
import Time exposing (Time)


type alias Model =
    { tasks : Tasks.Model
    , users : Users.Model
    , printTasks : PrintTasks.Model
    }


initialModel : Model
initialModel =
    { tasks = Tasks.initialModel
    , printTasks = PrintTasks.initialModel
    , users = Users.initialModel
    }


type Msg
    = ViewState (Maybe SettingsSubPage)
    | TimeTick Time
    | Tasks Tasks.Msg
    | PrintTasks PrintTasks.Msg
    | Users Users.Msg


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        ViewState subPage ->
            model !: (List.map send <| viewStateMsgs subPage)

        TimeTick time ->
            update (PrintTasks <| PrintTasks.TimeTick time) model globals

        Tasks tmsg ->
            let
                ( newModel, cmd, globalsCmd ) =
                    Tasks.update tmsg model.tasks globals
            in
            { model | tasks = newModel } !> ( [ Cmd.map Tasks cmd ], [ globalsCmd ] )

        PrintTasks tmsg ->
            let
                ( newModel, cmd, globalsCmd ) =
                    PrintTasks.update tmsg model.printTasks globals
            in
            { model | printTasks = newModel } !> ( [ Cmd.map PrintTasks cmd ], [ globalsCmd ] )

        Users umsg ->
            let
                ( newModel, cmd, globalsCmd ) =
                    Users.update umsg model.users globals
            in
            { model | users = newModel } !> ( [ Cmd.map Users cmd ], [ globalsCmd ] )


viewStateMsgs : Maybe SettingsSubPage -> List Msg
viewStateMsgs subPage =
    case subPage of
        Just SettingsMainPage ->
            [ Tasks <| Tasks.ViewState False
            , Users <| Users.ViewState False
            , PrintTasks <| PrintTasks.ViewState False
            ]

        Just SettingsTasksPage ->
            [ Tasks <| Tasks.ViewState True
            , Users <| Users.ViewState False
            , PrintTasks <| PrintTasks.ViewState False
            ]

        Just SettingsUsersPage ->
            [ Tasks <| Tasks.ViewState False
            , Users <| Users.ViewState True
            , PrintTasks <| PrintTasks.ViewState False
            ]

        Just TasksPrintPage ->
            [ Tasks <| Tasks.ViewState False
            , Users <| Users.ViewState False
            , PrintTasks <| PrintTasks.ViewState True
            ]

        Nothing ->
            [ Tasks <| Tasks.ViewState False
            , Users <| Users.ViewState False
            , PrintTasks <| PrintTasks.ViewState False
            ]


view : SettingsSubPage -> Model -> Globals.Types.Model -> ( Html Msg, Html Msg, Bool )
view page model globals =
    let
        ( content, footerContent, navShow ) =
            viewContent page model globals
    in
    ( Grid.container []
        [ Grid.row []
            (case navShow of
                True ->
                    [ Grid.col
                        [ Col.xs12, Col.md2, Col.attrs [ class "bd-sidebar" ] ]
                        [ viewSidebar page ]
                    , Grid.col
                        [ Col.xs12, Col.md10, Col.attrs [ class "bd-content" ] ]
                        [ content ]
                    ]

                False ->
                    [ Grid.col
                        [ Col.xs12, Col.md12, Col.attrs [ class "bd-content" ] ]
                        [ content ]
                    ]
            )
        ]
    , footerContent
    , navShow
    )


defaultPageBehaviour : Html Msg -> ( Html Msg, Html msg, Bool )
defaultPageBehaviour bodyContent =
    ( bodyContent, text "", True )


viewContent : SettingsSubPage -> Model -> Globals.Types.Model -> ( Html Msg, Html Msg, Bool )
viewContent page model globals =
    case page of
        SettingsMainPage ->
            defaultPageBehaviour <| text "SettingsMainPage"

        SettingsTasksPage ->
            defaultPageBehaviour <| Html.map Tasks <| Tasks.view model.tasks globals

        SettingsUsersPage ->
            defaultPageBehaviour <| Html.map Users <| Users.view model.users globals

        TasksPrintPage ->
            let
                ( body, footer, navView ) =
                    PrintTasks.view model.printTasks globals
            in
            ( Html.map PrintTasks body, Html.map PrintTasks footer, navView )


viewSidebar : SettingsSubPage -> Html msg
viewSidebar page =
    let
        sidebarLink : String -> SettingsSubPage -> SettingsSubPage -> Html msg
        sidebarLink title activePage currentPage =
            li
                (if activePage == currentPage then
                    [ class "active bd-sidenav-active" ]
                 else
                    []
                )
                [ a [ href <| pageToUrl activePage ] [ text title ] ]
    in
    nav [ class "bd-links" ]
        [ div [ class "bd-toc-item active" ]
            [ text "Navigation"
            , ul [ class "nav bd-sidenav" ]
                [ sidebarLink "Tasks" SettingsTasksPage page
                , sidebarLink "Users" SettingsUsersPage page
                ]
            ]
        ]


pageToUrl : SettingsSubPage -> String
pageToUrl subPage =
    let
        subPageUrl =
            case subPage of
                SettingsMainPage ->
                    ""

                SettingsTasksPage ->
                    "tasks"

                TasksPrintPage ->
                    "tasks/pdf-export"

                SettingsUsersPage ->
                    "users"
    in
    "#settings/" ++ subPageUrl

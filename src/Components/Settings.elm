module Components.Settings exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Components.Settings.Tasks as Tasks
import Globals.Types
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Functions exposing (send)
import Html exposing (Html, a, div, li, nav, text, ul)
import Html.Attributes exposing (class, href)
import Pages exposing (SettingsSubPage(..))


type alias Model =
    { tasks : Tasks.Model
    }


initialModel : Model
initialModel =
    { tasks = Tasks.initialModel
    }


type Msg
    = ViewState (Maybe SettingsSubPage)
    | Tasks Tasks.Msg


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        ViewState subPage ->
            model !: (List.map send <| viewStateMsgs subPage)

        Tasks tmsg ->
            let
                ( newModel, cmd, globalsCmd ) =
                    Tasks.update tmsg model.tasks globals
            in
            { model | tasks = newModel } !> ( [ Cmd.map Tasks cmd ], [ globalsCmd ] )

viewStateMsgs : Maybe SettingsSubPage -> List Msg
viewStateMsgs subPage =
    case subPage of
        Just SettingsMainPage ->
            [ Tasks <| Tasks.ViewState False
            ]
        Just SettingsTasksPage ->
            [ Tasks <| Tasks.ViewState True
            ]
        Nothing ->
            [ Tasks <| Tasks.ViewState False
            ]


view : SettingsSubPage -> Model -> Globals.Types.Model -> Html msg
view page model globals =
    Grid.container []
        [ Grid.row []
            [ Grid.col
                [ Col.xs12, Col.md2, Col.attrs [ class "bd-sidebar" ] ]
                [ viewSidebar page ]
            , Grid.col
                [ Col.xs12, Col.md10, Col.attrs [ class "bd-content" ] ]
                [ viewContent page model globals ]
            ]
        ]

viewContent : SettingsSubPage -> Model -> Globals.Types.Model -> Html msg
viewContent page model globals =
    case page of
        SettingsMainPage ->
            text "SettingsMainPage"
        SettingsTasksPage ->
            Tasks.view model.tasks globals



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
                [ sidebarLink "Tasks" SettingsTasksPage page ]
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
    in
    "#settings/" ++ subPageUrl

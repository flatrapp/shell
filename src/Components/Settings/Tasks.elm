module Components.Settings.Tasks exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Globals.Types
import Helpers.Operators exposing ((!:), (!>))
import Html exposing (Html, text, ul, div, nav, a, li)
import Html.Attributes exposing (class, href)


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


type Msg
    = ViewState Bool


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        ViewState state ->
            model !: []


view : Model -> Globals.Types.Model -> Html msg
view model globals =
    text "Tasks Page"

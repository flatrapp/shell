module Components.Dashboard exposing (..)

import Globals.Types
import Html exposing (Html, text)


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


type Msg
    = Nomsg
    | ViewState Bool


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg )
update msg model globals =
    case msg of
        Nomsg ->
            model ! []

        ViewState state ->
            model ! []


view : Model -> Globals.Types.Model -> Html msg
view model globals =
    text "Dashboard"

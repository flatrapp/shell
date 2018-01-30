module Components.Dashboard exposing (..)

import Globals exposing (Model)
import Html exposing (Html, text)


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


type Msg
    = Nomsg


update : Msg -> Model -> Globals.Model -> ( Model, Cmd Msg )
update msg model globals =
    case msg of
        Nomsg ->
            model ! []


view : Model -> Globals.Model -> Html msg
view model globals =
    text "Dashboard"

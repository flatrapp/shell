module Page.Dashboard exposing (render)

import Html exposing (Html, div, text)
import Models exposing (Model)
import Msgs exposing (Msg(..))


render : Model -> Html Msg
render model =
    div []
        [ text "this comes from the dashboard" ]

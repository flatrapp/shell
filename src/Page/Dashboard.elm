module Page.Dashboard exposing (render)

import Models exposing (Model)
import Msgs exposing (Msg(..))
import Html exposing (Html, div, text)


render : Model -> Html Msg
render model =
    div []
        [ text "this comes from the dashboard" ]

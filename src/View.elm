module View exposing (..)

import Models exposing (Model, Page(..))
import Msgs exposing (Msg(..))

import Page
import Page.Dashboard
import Page.NotFound

import Html exposing (Html, div, text)

view : Model -> Html Msg
view model =
    Page.render model (renderPage model.page model)

renderPage : Page -> Model -> Html Msg
renderPage page model =
    case page of
        Dashboard ->
            Page.Dashboard.render model
        NotFound ->
            Page.NotFound.render model

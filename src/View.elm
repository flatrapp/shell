module View exposing (..)

import Models exposing (Model, Page(..))
import Msgs exposing (Msg(..))
import Authentication exposing (checkAuthenticated)
import Page
import Page.Login
import Page.Dashboard
import Page.NotFound
import Html exposing (Html, div, text)


view : Model -> Html Msg
view model =
    let
        -- If not logged in, don't display navigation items
        navEnabled =
            checkAuthenticated model.time model.auth
    in
        Page.render navEnabled model (renderPage model.page model)


renderPage : Page -> Model -> Html Msg
renderPage page model =
    case page of
        Dashboard ->
            Page.Dashboard.render model

        Login ->
            Page.Login.render model

        NotFound ->
            Page.NotFound.render model

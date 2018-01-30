module View exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar exposing (..)
import Components.Dashboard exposing (view)
import Components.Login exposing (view)
import Globals.Types
import Helpers.Authentication exposing (isAuthenticated)
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onSubmit)
import Model exposing (Model)
import Msg exposing (Msg)
import Pages exposing (..)
import Time


view : Model.Model -> Html Msg.Msg
view model =
    div []
        [ menu (isAuthenticated model.globals) model
        , br [] []
        , Grid.container [] [ content model ]
        ]


content : Model.Model -> Html Msg.Msg
content model =
    case model.globals.page of
        NotFoundPage ->
            text "Site not found"

        LoginPage ->
            Html.map Msg.Login (Components.Login.view model.login model.globals)

        DashboardPage ->
            Html.map Msg.Dashboard (Components.Dashboard.view model.dashboard model.globals)


menu : Bool -> Model.Model -> Html Msg.Msg
menu navigationEnabled model =
    Navbar.config Msg.NavbarEvent
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "flatr" ]
        |> Navbar.items (navbarItems navigationEnabled)
        |> navba navigationEnabled
        |> Navbar.view model.navState


navbarItems : Bool -> List (Navbar.Item msg)
navbarItems itemsVisible =
    if itemsVisible then
        [ Navbar.itemLink [ href "#" ] [ text "Dashboard" ]
        , Navbar.itemLink [ href "#cleaning-schedule" ] [ text "Cleaning Schedule" ]
        ]
    else
        []


navba : Bool -> Config Msg -> Config Msg
navba authenticated config =
    if authenticated then
        config
            |> Navbar.items (navbarItems True)
            |> Navbar.customItems
                [ Navbar.textItem [ class "muted ml-sm-2", style [ ( "margin-right", "20px" ) ] ] [ text ("Session-Timeout: " ++ "blubb" ++ " minutes") ]
                , Navbar.formItem [ onSubmit (Msg.Globals Globals.Types.Logout) ]
                    [ Button.button [] [ text "Logout" ]
                    ]
                ]
    else
        config


remainingTime : Maybe Time.Time -> Float -> Int
remainingTime maybeTime until =
    case maybeTime of
        Nothing ->
            -1

        Just time ->
            round ((until - time) / Time.minute)

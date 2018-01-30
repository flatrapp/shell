module View exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar exposing (..)
import Components.Dashboard exposing (view)
import Components.Login exposing (view)
import Components.NotFound
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
    case model.globals.time of
        Nothing ->
            div []
                [ menu False model
                , br [] []
                , Grid.container [] [ h1 [] [ text "Loading..." ] ]
                ]

        Just _ ->
            div []
                [ menu (isAuthenticated model.globals) model
                , br [] []
                , Grid.container [] [ content model ]
                ]


content : Model.Model -> Html Msg.Msg
content model =
    case model.globals.page of
        NotFoundPage ->
            Html.map Msg.Login Components.NotFound.view

        LoginPage ->
            Html.map Msg.Login (Components.Login.view model.login model.globals)

        DashboardPage ->
            Html.map Msg.Dashboard (Components.Dashboard.view model.dashboard model.globals)


menu : Bool -> Model.Model -> Html Msg.Msg
menu navigationEnabled model =
    Navbar.config Msg.NavbarEvent
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "flatr" ]
        |> navbarItems navigationEnabled model
        |> Navbar.view model.navState


navbarItems : Bool -> Model.Model -> Config Msg -> Config Msg
navbarItems authenticated model config =
    if authenticated then
        config
            |> Navbar.items
                [ Navbar.itemLink [ href "#" ] [ text "Dashboard" ]
                , Navbar.itemLink [ href "#cleaning-schedule" ] [ text "Cleaning Schedule" ]
                ]
            |> Navbar.customItems
                [ Navbar.textItem
                    [ class "muted ml-sm-2"
                    , style [ ( "margin-right", "20px" ) ]
                    ]
                    [ text ("Session-Timeout: " ++ (toString <| remainingTime model) ++ " minutes") ]
                , Navbar.formItem [ onSubmit (Msg.Globals Globals.Types.Logout) ]
                    [ Button.button [] [ text "Logout" ]
                    ]
                ]
    else
        config


remainingTime : Model.Model -> Int
remainingTime model =
    case model.globals.time of
        Nothing ->
            -1

        Just time ->
            case model.globals.auth of
                Nothing ->
                    -1

                Just auth ->
                    truncate ((auth.validUntil - time) / Time.minute)

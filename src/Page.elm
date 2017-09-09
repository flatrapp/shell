module Page exposing (render)

import Msgs exposing (Msg(..))
import Models exposing (Model)
import Routing exposing (dashboardPath, cleaningSchedulePath)
import Html exposing (Html, div, text, br)
import Html.Attributes exposing (href)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid


render : Bool -> Model -> Html Msg -> Html Msg
render navigationEnabled model subpage =
    div []
        [ menu navigationEnabled model
        , br [] []
        , mainContent subpage
        ]


menu : Bool -> Model -> Html Msg
menu navigationEnabled model =
    Navbar.config OnNavbarEvent
        |> Navbar.container
        |> Navbar.brand [ href dashboardPath ] [ text "flatr" ]
        |> Navbar.items (navbarItems navigationEnabled)
        |> Navbar.view model.navState


navbarItems : Bool -> List (Navbar.Item msg)
navbarItems itemsVisible =
    if itemsVisible then
        [ Navbar.itemLink [ href dashboardPath ] [ text "Dashboard" ]
        , Navbar.itemLink [ href cleaningSchedulePath ] [ text "Cleaning Schedule" ]
        ]
    else
        []


mainContent : Html Msg -> Html Msg
mainContent subpage =
    Grid.container [] [ subpage ]

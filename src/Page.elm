module Page exposing (render)

import Msgs exposing (Msg(..))
import Models exposing (Model)
import Routing exposing (dashboardPath, cleaningSchedulePath)
import Html exposing (Html, div, text, br)
import Html.Attributes exposing (href)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid


render : Model -> Html Msg -> Html Msg
render model subpage =
    div []
        [ menu model
        , br [] []
        , mainContent subpage
        ]


menu : Model -> Html Msg
menu model =
    Navbar.config OnNavbarEvent
        |> Navbar.container
        |> Navbar.brand [ href dashboardPath ] [ text "flatr" ]
        |> Navbar.items
            [ Navbar.itemLink [ href dashboardPath ] [ text "Dashboard" ]
            , Navbar.itemLink [ href cleaningSchedulePath ] [ text "Cleaning Schedule" ]
            ]
        |> Navbar.view model.navState


mainContent : Html Msg -> Html Msg
mainContent subpage =
    Grid.container [] [ subpage ]

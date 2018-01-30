module Components.NotFound exposing (..)

import Html exposing (Html, text, br, a, p)
import Html.Attributes exposing (href)
import Bootstrap.Alert as Alert

view : Html msg
view =
    Alert.danger
        [ Alert.h4 [] [ text "Not found" ]
        , p []
            [ text "The resource you requested can't be found."
            , br [] []
            , text "Click "
            , a [ href "#" ] [ text "here" ]
            , text " to go back to the dashboard."
            ]
        , p [] [ text "If you expected something else to be here, please report this incident." ]
        ]

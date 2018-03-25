module View exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar exposing (..)
import Components.Dashboard
import Components.Login
import Components.NotFound
import Components.Settings
import Components.Signup
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
                , Grid.container []
                    [ Grid.row []
                        [ Grid.col [] [ h1 [] [ text "Loading..." ] ] ]
                    , Grid.row []
                        [ Grid.col []
                            [ br [] []
                            , br [] []
                            , hr [] []
                            , footer model.globals <| text ""
                            ]
                        ]
                    ]
                ]

        Just _ ->
            let
                ( bodyContent, footerContent, showNav) =
                    content model
            in
            div []
                [ menu ((isAuthenticated model.globals) && showNav) model
                , br [] []
                , Grid.container []
                    [ Grid.row []
                        [ Grid.col [] [ bodyContent ] ]
                    , Grid.row []
                        [ Grid.col []
                            [ br [] []
                            , br [] []
                            , hr [] []
                            , footer model.globals footerContent
                            ]
                        ]
                    ]
                ]


content : Model.Model -> ( Html Msg.Msg, Html Msg.Msg, Bool )
content model =
    case model.globals.page of
        NotFoundPage ->
            ( Html.map Msg.Login Components.NotFound.view, text "", True )

        LoginPage ->
            ( Html.map Msg.Login (Components.Login.view model.login model.globals), text "", True )

        SignupPage ->
            ( Html.map Msg.Signup (Components.Signup.view model.signup model.globals), text "", True )

        DashboardPage ->
            let
                ( bodyContent, footerContent ) =
                    Components.Dashboard.view model.dashboard model.globals
            in
            ( Html.map Msg.Dashboard bodyContent, Html.map Msg.Dashboard footerContent, True )

        SettingsPage page ->
            let
                ( bodyContent, footerContent, showNav ) =
                    Components.Settings.view page model.settings model.globals
            in
            ( Html.map Msg.Settings bodyContent, Html.map Msg.Settings footerContent, showNav )


footer : Globals.Types.Model -> Html msg -> Html msg
footer globals customFooter =
    div [ style <| List.singleton ( "float", "clear" ) ]
        [ div [ style [ ( "float", "left" ), ( "margin-right", "30px" ) ] ]
            [ p []
                ([ text <| "flatr - "
                 , code []
                    [ text "shell"
                    , text " v"
                    , text globals.version
                    ]
                 ]
                    ++ (case globals.serverInfo of
                            Nothing ->
                                []

                            Just info ->
                                if isAuthenticated globals then
                                    [ text " - connected to "
                                    , code []
                                        [ text info.name
                                        , text " "
                                        , text info.version
                                        ]
                                    ]
                                else
                                    []
                       )
                )
            ]
        , div [ style <| List.singleton ( "float", "right" ) ] [ customFooter ]
        ]


menu : Bool -> Model.Model -> Html Msg.Msg
menu navigationEnabled model =
    Navbar.config Msg.NavbarEvent
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "flatr" ]
        |> navbarItems navigationEnabled model
        |> Navbar.view model.navState


itemLinkDynamic : Bool -> List (Html.Attribute msg) -> List (Html.Html msg) -> Navbar.Item msg
itemLinkDynamic active attrs elems =
    if active then
        Navbar.itemLinkActive attrs elems
    else
        Navbar.itemLink attrs elems


navbarItems : Bool -> Model.Model -> Config Msg -> Config Msg
navbarItems authenticated model config =
    if authenticated then
        config
            |> Navbar.items
                [ itemLinkDynamic (DashboardPage == model.globals.page) [ href "#" ] [ text "Dashboard" ]
                , itemLinkDynamic
                    (case model.globals.page of
                        SettingsPage _ ->
                            True

                        _ ->
                            False
                    )
                    [ href "#settings" ]
                    [ text "Settings" ]
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

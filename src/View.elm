module View exposing (..)

import Html exposing (Html, div, text)
import Msgs exposing (Msg)
import Models exposing (Model, PlayerId)
import Players.List
import Players.Edit
import RemoteData exposing (WebData)

view : Model -> Html Msg
view model =
    div []
        [ page model ]

page : Model -> Html Msg
page model =
    case model.route of
        Models.PlayersRoute ->
            Players.List.view model.players
        Models.PlayerRoute playerId ->
            playerEditPage model playerId
        Models.NotFoundRoute ->
            notFoundView

playerEditPage : Model -> PlayerId -> Html Msg
playerEditPage model id =
    case model.players of
        RemoteData.NotAsked ->
            text "Preparing to load..."
        RemoteData.Loading ->
            text "Loading..."
        RemoteData.Failure err ->
            text (toString err)
        RemoteData.Success players ->
            let
                maybePlayer =
                    players
                        |> List.filter (\player -> player.id == id)
                        |> List.head
            in
               case maybePlayer of
                   Just player ->
                       Players.Edit.view player
                   Nothing ->
                       playerNotFoundView id

notFoundView : Html Msg
notFoundView =
    div []
        [ text "Nothing here, sorry... :'(" ]

playerNotFoundView : PlayerId -> Html Msg
playerNotFoundView id =
    div []
        [ text <| "Player with id " ++ id ++ " not found... :'(" ]

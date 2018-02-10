module Components.Settings.Users exposing (..)

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Dict exposing (Dict)
import Globals.Types
import Helpers.Operators exposing ((!:), (!>))
import Helpers.User as User exposing (..)
import Html exposing (Html, a, br, div, h2, hr, text)
import Html.Attributes exposing (class, href, style)
import Http


type alias Model =
    { users : Maybe (Dict Int UserInfo)
    }


initialModel : Model
initialModel =
    { users = Nothing
    }


type Msg
    = ViewState Bool
    | UpdateData
    | ListUsersResponse (Result Http.Error User.ListUsersResponse)


update : Msg -> Model -> Globals.Types.Model -> ( Model, Cmd Msg, Cmd Globals.Types.Msg )
update msg model globals =
    case msg of
        ViewState state ->
            if state then
                update UpdateData model globals
            else
                model !: []

        UpdateData ->
            case globals.auth of
                Nothing ->
                    model !: []

                Just auth ->
                    model
                        !: [ Http.send ListUsersResponse <| listUsersRequest auth
                           ]

        ListUsersResponse res ->
            case listUsersResponseDecode res of
                ListUsersSuccessResponse users ->
                    { model | users = Just users } !: []

                _ ->
                    model !: []


view : Model -> Globals.Types.Model -> Html msg
view model globals =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ h2 [] [ text "Users" ]
                , hr [] []
                , br [] []
                ]
            ]
        , Grid.row []
            [ Grid.col [] [ usersList model.users ] ]
        ]


usersList : Maybe (Dict Int UserInfo) -> Html msg
usersList maybeUsers =
    case maybeUsers of
        Nothing ->
            ListGroup.ul
                [ ListGroup.li
                    [ ListGroup.attrs [ class "justify-content-between" ] ]
                    [ text "Loading users..." ]
                ]

        Just users ->
            Dict.values users
                |> List.map usersListEntry
                |> ListGroup.ul


usersListEntry : UserInfo -> ListGroup.Item msg
usersListEntry user =
    ListGroup.li
        ([ ListGroup.attrs [ class "justify-content-between" ]
         ]
            ++ (if user.absent then
                    [ ListGroup.warning ]
                else
                    []
               )
        )
        [ div []
            [ text <| user.firstName ++ " " ++ user.lastName
            , if user.absent then
                Badge.badge [ style [ ( "margin-left", "20px" ) ] ] [ text "Absent" ]
              else
                text ""
            , br [] []
            , a [ href <| "mailto:" ++ user.email ] [ text user.email ]
            ]
        , Button.button
            [ Button.small, Button.danger, Button.disabled True, Button.attrs [ class "ml-1" ] ]
            [ text "Disable user" ]
        ]

module Components.Settings.Users exposing (..)

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Dict exposing (Dict)
import Globals.Types
import Helpers.Api.Invitation as Invitation exposing (..)
import Helpers.Api.User as User exposing (..)
import Helpers.Functions exposing (..)
import Helpers.Operators exposing ((!:), (!>))
import Helpers.Toast as Toast exposing (errorToast, successToast)
import Html exposing (Html, a, br, div, h2, h3, hr, i, text)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onSubmit)
import Http


type alias Model =
    { users : Maybe (Dict Int UserInfo)
    , invitationEmail : String
    , invitations : Maybe (Dict Int Invitation)
    }


initialModel : Model
initialModel =
    { users = Nothing
    , invitationEmail = ""
    , invitations = Nothing
    }


type Msg
    = ViewState Bool
    | UpdateData
    | InvitationEmailChange String
    | CreateInvitation
    | ResendInvitation Int
    | DeleteInvitation Int
    | ListUsersResponse (Result Http.Error User.ListUsersResponse)
    | ListInvitationsResponse (Result Http.Error Invitation.ListInvitationsResponse)
    | CreateInvitationResponse (Result Http.Error Invitation.CreateInvitationResponse)
    | ResendInvitationResponse (Result Http.Error Invitation.ResendInvitationResponse)
    | DeleteInvitationResponse (Result Http.Error Invitation.DeleteInvitationResponse)


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
                           , Http.send ListInvitationsResponse <| listInvitationsRequest auth
                           ]

        ListUsersResponse res ->
            case listUsersResponseDecode res of
                ListUsersSuccessResponse users ->
                    { model | users = Just users } !: []

                _ ->
                    -- TODO: Handle errors!
                    model !: []

        ListInvitationsResponse res ->
            case listInvitationsResponseDecode res of
                ListInvitationsSuccessResponse invitations ->
                    { model | invitations = Just invitations } !: []

                _ ->
                    -- TODO: Handle errors!
                    model !: []

        InvitationEmailChange email ->
            { model | invitationEmail = email } !: []

        CreateInvitation ->
            case globals.auth of
                Nothing ->
                    model !: []

                Just auth ->
                    model !: [ Http.send CreateInvitationResponse <| createInvitationRequest auth model.invitationEmail ]

        ResendInvitation id ->
            case globals.auth of
                Nothing ->
                    model !: []

                Just auth ->
                    model !: [ Http.send ResendInvitationResponse <| resendInvitationRequest auth id ]

        DeleteInvitation id ->
            case globals.auth of
                Nothing ->
                    model !: []

                Just auth ->
                    model !: [ Http.send DeleteInvitationResponse <| deleteInvitationRequest auth id ]

        CreateInvitationResponse res ->
            case createInvitationResponseDecode res of
                CreateInvitationSuccessResponse invitation ->
                    case model.invitations of
                        Nothing ->
                            -- Load the invitations again, clearly something went wrong
                            model !: [ send UpdateData ]

                        Just invitations ->
                            { model
                                | invitations = Just <| Dict.insert invitation.id invitation invitations
                                , invitationEmail = ""
                            }
                                !: []

                _ ->
                    -- TODO: Handle errors!
                    model !: []

        ResendInvitationResponse res ->
            case resendInvitationResponseDecode res of
                ResendInvitationSuccessResponse ->
                    model !: [ successToast "Invitation email resent" "" ]

                _ ->
                    -- TODO: Handle errors!
                    model !: []

        DeleteInvitationResponse res ->
            case deleteInvitationResponseDecode res of
                DeleteInvitationSuccessResponse id ->
                    case model.invitations of
                        Nothing ->
                            -- Load the invitations again, clearly something went wrong
                            -- This should not happen at this stage, poor recovery attempt
                            model !: [ send UpdateData ]

                        Just invitations ->
                            { model | invitations = Just <| Dict.remove id invitations }
                                !: [ successToast "Invitation deleted" ""
                                   ]

                _ ->
                    -- TODO: Handle errors!
                    model !: []


view : Model -> Globals.Types.Model -> Html Msg
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
        , Grid.row []
            [ Grid.col []
                [ br [] []
                , br [] []
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ h2 [] [ text "Invitations" ]
                , hr [] []
                , br [] []
                ]
            ]
        , Grid.row []
            [ Grid.col [] [ createInvitationView model ] ]
        , Grid.row [] [ Grid.col [] [ br [] [] ] ]
        , Grid.row []
            [ Grid.col [] [ invitationsList model.invitations ] ]
        ]


createInvitationView : Model -> Html Msg
createInvitationView model =
    Form.form [ onSubmit CreateInvitation ]
        [ InputGroup.config
            (InputGroup.email
                [ Input.placeholder "Email"
                , Input.onInput InvitationEmailChange
                , Input.value model.invitationEmail
                ]
            )
            |> InputGroup.successors
                [ InputGroup.button [ Button.primary ] [ text "Send invitation" ] ]
            |> InputGroup.view
        ]


invitationsList : Maybe (Dict Int Invitation) -> Html Msg
invitationsList maybeInvitations =
    case maybeInvitations of
        Nothing ->
            ListGroup.ul
                [ ListGroup.li
                    [ ListGroup.attrs [ class "justify-content-between" ] ]
                    [ text "Loading invitations..." ]
                ]

        Just invitations ->
            if Dict.isEmpty invitations then
                ListGroup.ul
                    [ ListGroup.li
                        [ ListGroup.attrs [ class "justify-content-between" ] ]
                        [ text "No pending invitations. Why not invite someone friendly?" ]
                    ]
            else
                Dict.values invitations
                    |> List.map invitationsListEntry
                    |> ListGroup.ul


invitationsListEntry : Invitation -> ListGroup.Item Msg
invitationsListEntry invitation =
    ListGroup.li
        [ ListGroup.attrs [ class "justify-content-between" ] ]
        [ div [] [ i [] [ text invitation.email ] ]
        , div []
            [ Button.button
                [ Button.small
                , Button.warning
                , Button.attrs [ class "ml-1" ]
                , Button.onClick <| ResendInvitation invitation.id
                ]
                [ text "Resend Email" ]
            , Button.button
                [ Button.small
                , Button.danger
                , Button.attrs [ class "ml-1" ]
                , Button.onClick <| DeleteInvitation invitation.id
                ]
                [ text "Delete Invitation" ]
            ]
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
            , if not user.emailVerified then
                Badge.badge [ style [ ( "margin-left", "20px" ) ] ] [ text "Email not verified" ]
              else
                text ""
            , br [] []
            , a [ href <| "mailto:" ++ user.email ] [ text user.email ]
            ]
        , Button.button
            [ Button.small, Button.danger, Button.disabled True, Button.attrs [ class "ml-1" ] ]
            [ text "Disable user" ]
        ]

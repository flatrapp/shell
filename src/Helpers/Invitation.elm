port module Helpers.Invitation exposing (..)

import Dict exposing (Dict)
import Globals.Types exposing (Authentication)
import Helpers.Authentication exposing (authenticationHeaders)
import Helpers.Functions exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Json.Encode as Encode
import Time


requestTimeout : Float
requestTimeout =
    5 * Time.second



-- ////////// GENERIC //////////


type alias Invitation =
    { id : Int
    , email : String
    }


invitationDecoder : Decode.Decoder Invitation
invitationDecoder =
    DecodePipeline.decode
        (\id email -> { id = id, email = email })
        |> DecodePipeline.required "id" Decode.int
        |> DecodePipeline.required "email" Decode.string



-- ////////// CREATE INVITATION //////////


type CreateInvitationResponse
    = CreateInvitationSuccessResponse Invitation
    | CreateInvitationErrorResponse { error : CreateInvitationError, message : String }
    | CreateInvitationInvalidResponse
    | CreateInvitationHttpError Http.Error


type CreateInvitationError
    = CreateInvitationUnauthorizedError
    | CreateInvitationCollisionError
    | UnknownCreateInvitationError String


createInvitationRequest : Authentication -> String -> Http.Request CreateInvitationResponse
createInvitationRequest auth email =
    Http.request
        { body = createInvitationRequestEncode email |> Http.jsonBody
        , expect =
            Http.expectJson <|
                Decode.map CreateInvitationSuccessResponse invitationDecoder
        , headers = authenticationHeaders auth
        , method = "POST"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/invitations"
        , withCredentials = False
        }


createInvitationRequestEncode : String -> Encode.Value
createInvitationRequestEncode email =
    Encode.object <|
        [ ( "email", Encode.string email ) ]


createInvitationResponseDecode : Result Http.Error CreateInvitationResponse -> CreateInvitationResponse
createInvitationResponseDecode res =
    responseDecode
        createInvitationErrorDecoder
        CreateInvitationInvalidResponse
        CreateInvitationHttpError
        res


createInvitationErrorDecoder : Decode.Decoder CreateInvitationResponse
createInvitationErrorDecoder =
    errorDecoder
        (\code message ->
            CreateInvitationErrorResponse
                { error =
                    case code of
                        "unauthorized" ->
                            CreateInvitationUnauthorizedError

                        "invitation_email_exists" ->
                            CreateInvitationCollisionError

                        _ ->
                            UnknownCreateInvitationError code
                , message = message
                }
        )



-- ////////// LIST INVITATIONS //////////


type ListInvitationsResponse
    = ListInvitationsSuccessResponse (Dict Int Invitation)
    | ListInvitationsErrorResponse { error : ListInvitationsError, message : String }
    | ListInvitationsInvalidResponse
    | ListInvitationsHttpError Http.Error


type ListInvitationsError
    = ListInvitationsUnauthorizedError
    | UnknownListInvitationsError String


listInvitationsRequest : Authentication -> Http.Request ListInvitationsResponse
listInvitationsRequest auth =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson listInvitationsSuccessDecoder
        , headers = authenticationHeaders auth
        , method = "GET"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/invitations"
        , withCredentials = False
        }


listInvitationsResponseDecode : Result Http.Error ListInvitationsResponse -> ListInvitationsResponse
listInvitationsResponseDecode res =
    responseDecode
        listInvitationsErrorDecoder
        ListInvitationsInvalidResponse
        ListInvitationsHttpError
        res


listInvitationsSuccessDecoder : Decode.Decoder ListInvitationsResponse
listInvitationsSuccessDecoder =
    Decode.map
        (\invitations ->
            List.foldl (\invitation dict -> Dict.insert invitation.id invitation dict) Dict.empty invitations
                |> ListInvitationsSuccessResponse
        )
        (Decode.list invitationDecoder)


listInvitationsErrorDecoder : Decode.Decoder ListInvitationsResponse
listInvitationsErrorDecoder =
    errorDecoder <|
        \code message ->
            ListInvitationsErrorResponse
                { error =
                    case code of
                        "unauthorized" ->
                            ListInvitationsUnauthorizedError

                        _ ->
                            UnknownListInvitationsError code
                , message = message
                }



-- ////////// DELETE INVITATION //////////


type DeleteInvitationResponse
    = DeleteInvitationSuccessResponse Int
    | DeleteInvitationErrorResponse { error : DeleteInvitationError, message : String }
    | DeleteInvitationInvalidResponse
    | DeleteInvitationHttpError Http.Error


type DeleteInvitationError
    = DeleteInvitationUnauthorizedError
    | UnknownDeleteInvitationError String


deleteInvitationRequest : Authentication -> Int -> Http.Request DeleteInvitationResponse
deleteInvitationRequest auth invitationId =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok <| DeleteInvitationSuccessResponse invitationId)
        , headers = authenticationHeaders auth
        , method = "DELETE"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/invitations/" ++ toString invitationId
        , withCredentials = False
        }


deleteInvitationResponseDecode : Result Http.Error DeleteInvitationResponse -> DeleteInvitationResponse
deleteInvitationResponseDecode res =
    responseDecode
        deleteInvitationErrorDecoder
        DeleteInvitationInvalidResponse
        DeleteInvitationHttpError
        res


deleteInvitationErrorDecoder : Decode.Decoder DeleteInvitationResponse
deleteInvitationErrorDecoder =
    errorDecoder <|
        \code message ->
            DeleteInvitationErrorResponse
                { error =
                    case code of
                        "unauthorized" ->
                            DeleteInvitationUnauthorizedError

                        _ ->
                            -- "not_found"
                            UnknownDeleteInvitationError code
                , message = message
                }



-- ////////// RESEND INVITATION EMAIL //////////


type ResendInvitationResponse
    = ResendInvitationSuccessResponse
    | ResendInvitationErrorResponse { error : ResendInvitationError, message : String }
    | ResendInvitationInvalidResponse
    | ResendInvitationHttpError Http.Error


type ResendInvitationError
    = ResendInvitationUnauthorizedError
    | UnknownResendInvitationError String


resendInvitationRequest : Authentication -> Int -> Http.Request ResendInvitationResponse
resendInvitationRequest auth invitationId =
    Http.request
        { body = Http.jsonBody <| Encode.object []
        , expect = Http.expectStringResponse (\_ -> Ok ResendInvitationSuccessResponse)
        , headers = authenticationHeaders auth
        , method = "PATCH"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/invitations/" ++ toString invitationId
        , withCredentials = False
        }


resendInvitationResponseDecode : Result Http.Error ResendInvitationResponse -> ResendInvitationResponse
resendInvitationResponseDecode res =
    responseDecode
        resendInvitationErrorDecoder
        ResendInvitationInvalidResponse
        ResendInvitationHttpError
        res


resendInvitationErrorDecoder : Decode.Decoder ResendInvitationResponse
resendInvitationErrorDecoder =
    errorDecoder <|
        \code message ->
            ResendInvitationErrorResponse
                { error =
                    case code of
                        "unauthorized" ->
                            ResendInvitationUnauthorizedError

                        _ ->
                            -- "not_found", "bad_json"
                            UnknownResendInvitationError code
                , message = message
                }

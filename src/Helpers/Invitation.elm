port module Helpers.Invitation exposing (..)

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


port saveServerInput : String -> Cmd msg


port clearServerInput : () -> Cmd msg



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
    = UnknownCreateInvitationError String


createInvitationRequest : String -> String -> Authentication -> Http.Request CreateInvitationResponse
createInvitationRequest serverUrl email auth =
    Http.request
        { body = createInvitationRequestEncode email |> Http.jsonBody
        , expect =
            Http.expectJson <|
                Decode.map (\i -> CreateInvitationSuccessResponse i) invitationDecoder
        , headers = authenticationHeaders auth
        , method = "POST"
        , timeout = Just requestTimeout
        , url = serverUrl ++ "/invitations"
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
                        -- TODO: Parse errors for real
                        _ ->
                            UnknownCreateInvitationError code
                , message = message
                }
        )



-- ////////// LIST INVITATIONS //////////


type ListInvitationsResponse
    = ListInvitationsSuccessResponse (List Invitation)
    | ListInvitationsErrorResponse { error : ListInvitationsError, message : String }
    | ListInvitationsInvalidResponse
    | ListInvitationsHttpError Http.Error


type ListInvitationsError
    = UnknownListInvitationsError String


listInvitationsRequest : String -> Authentication -> Http.Request ListInvitationsResponse
listInvitationsRequest serverUrl auth =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson listInvitationsSuccessDecoder
        , headers = authenticationHeaders auth
        , method = "GET"
        , timeout = Just requestTimeout
        , url = serverUrl ++ "/invitations"
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
        (\invitations -> ListInvitationsSuccessResponse invitations)
        (Decode.list invitationDecoder)


listInvitationsErrorDecoder : Decode.Decoder ListInvitationsResponse
listInvitationsErrorDecoder =
    errorDecoder <|
        \code message ->
            ListInvitationsErrorResponse
                { error =
                    case code of
                        _ ->
                            UnknownListInvitationsError code
                , message = message
                }



-- ////////// DELETE INVITATION //////////

type DeleteInvitationResponse
    = DeleteInvitationSuccessResponse
    | DeleteInvitationErrorResponse { error : DeleteInvitationError, message : String }
    | DeleteInvitationInvalidResponse
    | DeleteInvitationHttpError Http.Error


type DeleteInvitationError
    = UnknownDeleteInvitationError String

deleteInvitationRequest : String -> Authentication -> Int -> Http.Request String
deleteInvitationRequest serverUrl auth invitationId =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectString
        , headers = authenticationHeaders auth
        , method = "DELETE"
        , timeout = Just requestTimeout
        , url = serverUrl ++ "/invitations/" ++ toString invitationId
        , withCredentials = False
        }


deleteInvitationResponseDecode : Result Http.Error DeleteInvitationResponse -> DeleteInvitationResponse
deleteInvitationResponseDecode res =
    flexibleResponseDecode
        -- Ignore response string, as long as it's in the 200-300 range
        (\_ -> DeleteInvitationSuccessResponse)
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
                        _ ->
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
    = UnknownResendInvitationError String

resendInvitationRequest : String -> Authentication -> Int -> Http.Request String
resendInvitationRequest serverUrl auth invitationId =
    Http.request
        { body = Http.jsonBody <| Encode.object []
        , expect = Http.expectString
        , headers = authenticationHeaders auth
        , method = "PATCH"
        , timeout = Just requestTimeout
        , url = serverUrl ++ "/invitations/" ++ toString invitationId
        , withCredentials = False
        }


resendInvitationResponseDecode : Result Http.Error ResendInvitationResponse -> ResendInvitationResponse
resendInvitationResponseDecode res =
    flexibleResponseDecode
        -- Ignore response string, as long as it's in the 200-300 range
        (\_ -> ResendInvitationSuccessResponse)
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
                        _ ->
                            UnknownResendInvitationError code
                , message = message
                }


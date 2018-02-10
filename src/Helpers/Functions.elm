module Helpers.Functions exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Task


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


errorDecoder : (String -> String -> a) -> Decode.Decoder a
errorDecoder errDec =
    DecodePipeline.decode
        errDec
        |> DecodePipeline.required "code" Decode.string
        |> DecodePipeline.required "message" Decode.string
        |> Decode.field "error"


flexibleResponseDecode : (b -> a) -> Decode.Decoder a -> a -> (Http.Error -> a) -> Result Http.Error b -> a
flexibleResponseDecode successDecoder badStatusDecoder parsingError httpError res =
    case res of
        Ok b ->
            successDecoder b

        Err (Http.BadStatus errRes) ->
            case Decode.decodeString badStatusDecoder errRes.body of
                Err _ ->
                    parsingError

                Ok a ->
                    a

        Err (Http.BadPayload _ _) ->
            parsingError

        Err a ->
            httpError a


responseDecode : Decode.Decoder a -> a -> (Http.Error -> a) -> Result Http.Error a -> a
responseDecode =
    flexibleResponseDecode (\a -> a)


maybeList : List (Maybe a) -> Maybe (List a)
maybeList maybeList =
    List.foldr
        (\current prev ->
            case prev of
                Nothing ->
                    Nothing

                Just list ->
                    case current of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just <| item :: list
        )
        (Just [])
        maybeList


maybe2 : ( Maybe a, Maybe b ) -> Maybe ( a, b )
maybe2 maybeTuple =
    case maybeTuple of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing


maybe3 : ( Maybe a, Maybe b, Maybe c ) -> Maybe ( a, b, c )
maybe3 maybeTuple =
    case maybeTuple of
        ( Just a, Just b, Just c ) ->
            Just ( a, b, c )

        _ ->
            Nothing


maybe4 : ( Maybe a, Maybe b, Maybe c, Maybe d ) -> Maybe ( a, b, c, d )
maybe4 maybeTuple =
    case maybeTuple of
        ( Just a, Just b, Just c, Just d ) ->
            Just ( a, b, c, d )

        _ ->
            Nothing

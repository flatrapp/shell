module Helpers.Functions exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline


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

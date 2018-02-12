port module Helpers.Toast exposing (Toast, errorToast, infoToast, sendToast, simpleToast, successToast)

import Json.Encode as Encode


port sendToastObject : Encode.Value -> Cmd msg


errorColor : String
errorColor =
    "red"


infoColor : String
infoColor =
    "blue"


successColor : String
successColor =
    "green"


type alias Toast =
    { message : String
    , title : String
    , timeout : Maybe Int
    , color : String
    }


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe encoder maybeVal =
    case maybeVal of
        Nothing ->
            Encode.null

        Just val ->
            encoder val


encodeToast : Toast -> Encode.Value
encodeToast t =
    Encode.object
        [ ( "message", Encode.string t.message )
        , ( "title", Encode.string t.title )
        , ( "color", Encode.string t.color )
        , ( "timeout", encodeMaybe Encode.int t.timeout )
        ]


sendToast : Toast -> Cmd msg
sendToast toast =
    sendToastObject <| encodeToast toast


simpleToast : String -> String -> Cmd msg
simpleToast title message =
    sendToastObject <|
        encodeToast
            { message = message
            , title = title
            , timeout = Nothing
            , color = infoColor
            }


errorToast : String -> String -> Cmd msg
errorToast title message =
    sendToastObject <|
        encodeToast
            { message = message
            , title = title
            , timeout = Nothing
            , color = errorColor
            }


infoToast : String -> String -> Cmd msg
infoToast title message =
    sendToastObject <|
        encodeToast
            { message = message
            , title = title
            , timeout = Nothing
            , color = infoColor
            }


successToast : String -> String -> Cmd msg
successToast title message =
    sendToastObject <|
        encodeToast
            { message = message
            , title = title
            , timeout = Nothing
            , color = successColor
            }

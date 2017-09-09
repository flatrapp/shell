module Authentication exposing (..)

import Time exposing (Time)


type alias Authentication =
    { token : String
    , validUntil : Time
    , username : String
    }


checkAuthenticated : Maybe Time -> Maybe Authentication -> Bool
checkAuthenticated maybeTime maybeAuth =
    case maybeTime of
        Nothing ->
            False

        Just time ->
            checkAuthenticated1 time maybeAuth


checkAuthenticated1 : Time -> Maybe Authentication -> Bool
checkAuthenticated1 time maybeAuth =
    case maybeAuth of
        Nothing ->
            False

        Just auth ->
            checkAuthenticated2 time auth


checkAuthenticated2 : Time -> Authentication -> Bool
checkAuthenticated2 time auth =
    if Time.inMilliseconds time > Time.inMilliseconds auth.validUntil then
        False
    else
        True

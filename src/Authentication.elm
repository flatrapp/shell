module Authentication exposing (..)

import Models exposing (Authentication, UnwrappedAuthenticationResponse)
import Time exposing (Time, second)


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


fromAuthenticationResponse : Time -> UnwrappedAuthenticationResponse -> Maybe Authentication
fromAuthenticationResponse time res =
    let
        validUntil =
            time + ((toFloat res.validFor) * second)
    in
        Just
            { token = res.token
            , tokenId = res.tokenId
            , validUntil = validUntil
            }

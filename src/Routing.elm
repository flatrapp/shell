module Routing exposing (..)

import Navigation exposing (Location)
import Models exposing (Page(..), Endpoint(..))
import UrlParser exposing (..)


matchers : Parser (Page -> a) a
matchers =
    oneOf
        [ map Dashboard top
        , map Login (s "login")
        ]


parseLocation : Location -> Page
parseLocation location =
    case (parseHash matchers location) of
        Just page ->
            page

        Nothing ->
            NotFound



-- Paths


loginPath : String
loginPath =
    "#login"


dashboardPath : String
dashboardPath =
    "#"


cleaningSchedulePath : String
cleaningSchedulePath =
    "#cleaningSchedule"



-- Urls


endpointUrl : Endpoint -> String
endpointUrl endpoint =
    let
        base =
            --"http://localhost:4000"
            "http://212.47.232.80:8123"
    in
        base
            ++ case endpoint of
                Auth ->
                    "/auth"

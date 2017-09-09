module Routing exposing (..)

import Navigation exposing (Location)
import Models exposing (Page(..))
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

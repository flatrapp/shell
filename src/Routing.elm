module Routing exposing (..)

import Navigation exposing (Location)
import Models exposing (Page(..))
import UrlParser exposing (..)

matchers : Parser (Page -> a) a
matchers =
    oneOf
        [ map Dashboard top
        ]

parseLocation : Location -> Page
parseLocation location =
    case (parseHash matchers location) of
        Just page ->
            page
        Nothing ->
            NotFound

-- Paths

dashboardPath : String
dashboardPath = "#"

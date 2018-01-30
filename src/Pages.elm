module Pages exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Page
    = LoginPage
    | DashboardPage
    | NotFoundPage



-- Routing


locationMatchers : Parser (Page -> a) a
locationMatchers =
    oneOf
        [ map DashboardPage top
        , map LoginPage (s "login")
        ]


parseLocation : Location -> Page
parseLocation location =
    case parseHash locationMatchers location of
        Just page ->
            page

        Nothing ->
            NotFoundPage

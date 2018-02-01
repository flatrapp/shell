module Pages exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Page
    = LoginPage
    | SignupPage
    | DashboardPage
    | NotFoundPage



-- Routing


locationMatchers : Parser (Page -> a) a
locationMatchers =
    oneOf
        [ map DashboardPage top
        , map LoginPage (s "login")
        , map SignupPage (s "signup")
        ]


parseLocation : Location -> Page
parseLocation location =
    case parseHash locationMatchers location of
        Just page ->
            page

        Nothing ->
            NotFoundPage

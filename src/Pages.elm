module Pages exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type SettingsSubPage
    = SettingsMainPage
    | SettingsTasksPage
    | TasksPrintPage
    | SettingsUsersPage


type Page
    = LoginPage
    | SignupPage
    | DashboardPage
    | SettingsPage SettingsSubPage
    | NotFoundPage



-- Routing


locationMatchers : Parser (Page -> a) a
locationMatchers =
    oneOf
        [ map DashboardPage top
        , map LoginPage <| s "login"
        , map SignupPage <| s "signup"
        , map (SettingsPage SettingsMainPage) <| s "settings"
        , map (SettingsPage SettingsTasksPage) <| s "settings" </> s "tasks"
        , map (SettingsPage TasksPrintPage) <| s "settings" </> s "tasks" </> s "pdf-export"
        , map (SettingsPage SettingsUsersPage) <| s "settings" </> s "users"
        ]


parseLocation : Location -> Page
parseLocation location =
    case parseHash locationMatchers location of
        Just page ->
            page

        Nothing ->
            NotFoundPage

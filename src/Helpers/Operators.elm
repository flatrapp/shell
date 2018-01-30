module Helpers.Operators exposing (..)


(!:) : model -> List (Cmd msg) -> ( model, Cmd msg, Cmd globalsMsg )
(!:) model msgs =
    ( model, Cmd.batch msgs, Cmd.batch [] )


(!>) : model -> ( List (Cmd msg), List (Cmd globalsMsg) ) -> ( model, Cmd msg, Cmd globalsMsg )
(!>) model ( msgs, globalsMsgs ) =
    ( model, Cmd.batch msgs, Cmd.batch globalsMsgs )


(:>) : ( model, List cmd ) -> (model -> ( model, cmd )) -> ( model, List cmd )
(:>) ( model, msgs ) updateFn =
    -- This operator can be used to send a Msg to multiple subcomponents by chaining it together
    let
        ( newModel, msg ) =
            updateFn model
    in
    ( newModel, msgs ++ [ msg ] )

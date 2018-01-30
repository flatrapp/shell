module Page.Login exposing (render)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input exposing (onInput)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (for, style)
import Html.Events exposing (onSubmit)
import Models exposing (Model)
import Msgs exposing (Msg(..))


render : Model -> Html Msg
render model =
    div []
        [ h1 [ style [ ( "margin-bottom", "1.2em" ) ] ] [ text "Login" ]
        , Form.form [ onSubmit RequestAuthentication ]
            [ Form.group []
                [ Form.label [ for "email" ] [ text "E-Mail:" ]
                , Input.email [ Input.id "email", onInput OnLoginFormEmailChange ]
                ]
            , Form.group []
                [ Form.label [ for "password" ] [ text "Password:" ]
                , Input.password [ Input.id "password", onInput OnLoginFormPasswordChange ]
                ]
            , Form.group []
                [ Form.help []
                    [ text "Your password will never be stored in plaintext. "
                    , text "Please make sure that this website is visited "
                    , text "over a secure HTTPS connection!"
                    ]
                ]
            , Button.button [ Button.primary ] [ text "Login" ]
            ]
        ]

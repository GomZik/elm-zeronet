module Main exposing ( main )

import ZeroNet
import ZeroNet.Command as Command exposing ( Command )

import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Html.Events exposing ( .. )

type Model = M Int

type Msg
  = Inc
  | Dec

main : ZeroNet.Program () Model Msg
main =
  ZeroNet.program
    { init = init
    , update = update
    , view = view
    }


init : () -> ZeroNet.Key -> ( Model, Command Msg )
init _ key = ( M 0, Command.None )

update : Msg -> Model -> ( Model, Command Msg )
update msg ( M counter ) =
  case msg of
    Inc -> ( M ( counter + 1 ), Command.None )
    Dec -> ( M ( counter - 1 ), Command.None )

view : Model -> Html Msg
view ( M counter ) =
  div []
    [ button [ type_ "button", onClick Dec ] [ text "-" ]
    , text <| "Counter: " ++ String.fromInt counter
    , button [ type_ "button", onClick Inc ] [ text "+" ]
    ]

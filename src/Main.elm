module Main exposing ( main )

import ZeroNet
import ZeroNet.Navigation as Nav
import ZeroNet.Command as Command exposing ( Command )

import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Html.Events exposing ( .. )

import Url exposing ( Url )

type Page = Page1 | Page2

type alias Model =
  { counter : Int
  , page : Page
  , key : Nav.Key
  }

type Msg
  = Inc
  | Dec
  | UrlRequest Nav.Request
  | UrlChange Url

main : ZeroNet.Program () Model Msg
main =
  ZeroNet.program
    { init = init
    , update = update
    , view = view
    , onUrlRequest = UrlRequest
    , onUrlChange = UrlChange
    }


init : () -> Nav.Key -> ( Model, Command Msg )
init _ key =
  ( { counter = 0
    , page = Page1
    , key = key
    }
  , Command.none )

update : Msg -> Model -> ( Model, Command Msg )
update msg model =
  -- case Debug.log "Main.Message: " msg of
  case msg of
    Inc -> ( { model | counter = model.counter + 1 }, Command.none )
    Dec -> ( { model | counter = model.counter - 1 }, Command.none )
    UrlRequest req ->
      case req of
        Nav.Internal u -> ( model, Nav.pushUrl model.key u.path )
        _ -> ( model, Command.none )
    UrlChange u ->
      ( model, Command.none )


viewCounter : Model -> Html Msg
viewCounter model =
  div []
    [ button [ type_ "button", onClick Dec ] [ text "-" ]
    , text <| "Counter: " ++ String.fromInt model.counter
    , button [ type_ "button", onClick Inc ] [ text "+" ]
    ]


viewRouterExample : Model -> Html Msg
viewRouterExample model =
  div []
    [ ul []
      [ li [] [ a [ href "?" ] [ text "Page 1" ] ]
      , li [] [ a [ href "?Page2" ] [ text "Page 2" ] ]
      , li [] [ a [ href "http://127.0.0.1:43110/1GitLiXB6t5r8vuU2zC6a8GYj9ME6HMQ4t" ] [ text "Zite: GitCenter" ] ]
      , li [] [ a [ href "https://github.com/" ] [ text "External: Github" ] ]
      ]
    , div []
      [ text <| case model.page of
          Page1 -> "Page1"
          Page2 -> "Page2"
      ]
    ]


view : Model -> Html Msg
view model =
  div []
    [ viewCounter model
    , viewRouterExample model
    ]

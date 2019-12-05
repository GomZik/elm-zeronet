module Main exposing ( main )

import ZeroNet
import ZeroNet.Navigation as Nav
import ZeroNet.Command as Command exposing ( Command )
import ZeroNet.Subscription as Subscription exposing ( Subscription )
import ZeroNet.Auth as Auth

import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Html.Events exposing ( .. )

import Url exposing ( Url )
import Url.Parser as P

type ExternalType = Zite String | Clearnet String

type Page = Page1 | Page2 | External ExternalType

router : P.Parser ( Page -> a ) a
router =
  P.oneOf
    [ P.map Page1 P.top
    , P.map Page2 ( P.s "Page2" )
    ]

parseUrl : Url -> Page
parseUrl =
  P.parse router >> Maybe.withDefault Page1

type alias Model =
  { counter : Int
  , page : Page
  , key : Nav.Key
  , auth : Maybe String
  }

type Msg
  = Inc
  | Dec
  | UrlRequest Nav.Request
  | UrlChange Url
  | LoadExternal ExternalType
  | Login Auth.CertDomains
  | CertChanged ( Maybe String )

main : ZeroNet.Program () Model Msg
main =
  ZeroNet.program
    { init = init
    , update = update
    , view = view
    , onUrlRequest = UrlRequest
    , onUrlChange = UrlChange
    , subscriptions = subscriptions
    }


init : () -> Nav.Key -> Url -> ( Model, Command Msg )
init _ key url =
  ( { counter = 0
    , page = parseUrl url
    , key = key
    , auth = Nothing
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
        Nav.Zite u -> ( { model | page = External ( Zite u ) }, Command.none )
        Nav.External u -> ( { model | page = External ( Clearnet u ) }, Command.none )
    UrlChange u ->
      ( { model | page = parseUrl u }, Command.none )
    LoadExternal et ->
      let
        url = case et of
          Zite u -> u
          Clearnet u -> u
      in
        ( model, Nav.load url )
    Login cs ->
      ( model, Auth.certSelect cs )
    CertChanged cert ->
      ( { model | auth = cert }, Command.none )


viewCounterExample : Model -> Html Msg
viewCounterExample model =
  div []
    [ h1 [] [ text "Counter" ]
    , button [ type_ "button", onClick Dec ] [ text "-" ]
    , text <| "Counter: " ++ String.fromInt model.counter
    , button [ type_ "button", onClick Inc ] [ text "+" ]
    ]


viewRouterExample : Model -> Html Msg
viewRouterExample model =
  div []
    [ h1 [] [ text "Routing" ]
    , ul []
      [ li [] [ a [ href "?" ] [ text "Page 1" ] ]
      , li [] [ a [ href "?Page2" ] [ text "Page 2" ] ]
      , li [] [ a [ href "/1GitLiXB6t5r8vuU2zC6a8GYj9ME6HMQ4t" ] [ text "Zite: GitCenter" ] ]
      , li [] [ a [ href "https://github.com/" ] [ text "External: Github" ] ]
      ]
    , div []
      [ text <| case model.page of
          Page1 -> "Page1"
          Page2 -> "Page2"
          External et -> "Do you realy whant to go to " ++ case et of
            Zite u -> "zite " ++ u
            Clearnet u -> "clearnet site " ++ u
      , case model.page of
        External et -> div []
          [ button [ type_ "button", onClick <| LoadExternal et ] [ text "yes" ]
          , button [ type_ "button" ] [ text "no" ]
          ]
        _ -> text ""
      ]
    ]


viewLoginExample : Model -> Html Msg
viewLoginExample model =
  div []
    [ h1 [] [ text "Cert Selection" ]
    , case model.auth of
        Nothing -> p [] [ text "You are not logged in." ]
        Just cert -> p [] [ text <| "Hello, " ++ cert ]
    , button [ type_ "button", onClick <| Login Auth.Any ] [ text "Login (ANY)" ]
    ]


view : Model -> Html Msg
view model =
  div []
    [ viewCounterExample model
    , viewRouterExample model
    , viewLoginExample model
    ]

subscriptions : Model -> Subscription Msg
subscriptions _ =
  Auth.certChange CertChanged

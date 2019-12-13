module Main exposing ( main )

import ZeroNet
import ZeroNet.Navigation as Nav
import ZeroNet.Command as Command exposing ( Command )
import ZeroNet.Subscription as Subscription exposing ( Subscription )
import ZeroNet.Database as Db
import ZeroNet.Files as Files
import ZeroNet.Auth as Auth
import ZeroNet.Data.User as User exposing ( User )
import ZeroNet.Site as Site exposing ( SiteInfo )
import ZeroNet.Notification as Notification

import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Html.Events exposing ( .. )

import Url exposing ( Url )
import Url.Parser as P

import Json.Encode as JE
import Json.Decode as JD

import Time

import Task

type Page
  = Index
  | Docs
  | Api
  | Guides

type alias Model =
  { navKey : Nav.Key
  , page : Page
  }

type Msg
  = UrlRequest Nav.Request
  | UrlChange Url

main : ZeroNet.Program () Model Msg
main =
  ZeroNet.program
    { init = init
    , update = update
    , view = view
    , subscriptions = always Subscription.none
    , onUrlRequest = UrlRequest
    , onUrlChange = UrlChange
    }


urlParser : P.Parser ( Page -> a ) a
urlParser =
  P.oneOf
    [ P.map Index P.top
    , P.map Docs ( P.s "Docs" )
    , P.map Api ( P.s "Api" )
    , P.map Guides ( P.s "Guides" )
    ]

parsePage : Url -> Page
parsePage u =
  P.parse urlParser u
    |> Maybe.withDefault Index

init : () -> Nav.Key -> Url -> ( Model, Command Msg )
init _ key url =
  (
    { navKey = key
    , page = parsePage url
    }
  , Command.none
  )

update : Msg -> Model -> ( Model, Command Msg )
update msg model =
  -- case Debug.log "Main.Message" msg of
  case msg of
    UrlRequest req ->
      case req of
        Nav.Internal u -> ( model, Nav.pushUrl model.navKey u.path )
        Nav.Zite u -> ( model, Nav.load u )
        Nav.External u -> ( model, Nav.load u )
    UrlChange u ->
      ( { model | page = parsePage u }, Command.none )

viewNav : Model -> Html Msg
viewNav model =
  nav [ class "navbar" ]
    [ div [ class "container" ]
      [ div [ class "navbar-brand" ]
        [ a [ classList [ ( "navbar-item", True ), ( "is-active", model.page == Index ) ], href "?" ] [ text "Elm-ZeroNet" ]
        ]
      , div [ class "navbar-menu" ]
        [ div [ class "navbar-end" ]
          [ a [ classList [ ( "navbar-item", True ), ( "is-active", model.page == Docs ) ], href "?Docs" ] [ text "Documentation" ]
          , a [ classList [ ( "navbar-item", True ), ( "is-active", model.page == Api ) ], href "?Api" ] [ text "API Reference" ]
          , a [ classList [ ( "navbar-item", True ), ( "is-active", model.page == Guides ) ], href "?Guides" ] [ text "Guides" ]
          ]
        ]
      ]
    ]

viewHero : Html Msg
viewHero =
  section [ class "hero is-fullheight-with-navbar is-primary" ]
    [ div [ class "hero-body" ]
      [ div [ class "container" ]
        [ h1 [ class "title animated fadeInUp faster" ] [ text "Elm-ZeroNet" ]
        , h2 [ class "subtitle animated fadeInUp delay-200ms faster" ] [ text "A toolkit to build Zites with Elm" ]
        ]
      ]
    ]

view : Model -> Html Msg
view model =
  div []
    [ viewNav model
    , case model.page of
        Index -> viewHero
        Docs -> viewDocs
        Api -> viewApi
        Guides -> viewGuides
    ]

viewDocs : Html Msg
viewDocs =
  section [ class "section" ]
    [ div [ class "container" ]
      [ div [ class "columns" ]
        [ div [class "column is-one-third"]
          [ text "TOC"
          ]
        , div [ class "column" ]
          [ text "Content"
          ]
        ]
      ]
    ]

type Article = Article String String

docs : List Article
docs =
  [ Article "Getting Started"
    <| "# Hello, world!\n" ++
    "\n" ++
    "Test"
  ]

viewApi = text ""

viewGuides : Html Msg
viewGuides =
  section [ class "section" ]
    [ div [ class "container" ]
      [ div [ class "columns" ]
        [ div [ class "column is-one-third" ]
          [ a [ href "#" ] [ text "Chat" ]
          ]
        , div [ class "column" ]
          [ h1 [ class "title" ] [ text "Making a simple chat" ]
          ]
        ]
      ]
    ]

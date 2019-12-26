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
import Url.Parser as P exposing ( (</>) )

import Json.Encode as JE
import Json.Decode as JD

import App.Data.Article as Article exposing ( Article )

import Time

import Task

type Route
  = Index
  | Docs
  | EditDocs Int
  | Api
  | Guides

type alias Model =
  { navKey : Nav.Key
  , route : Route
  , docs : WebData ( List Article )
  , siteInfo : WebData SiteInfo
  }

type DocsPage
  = DocsIndexView
  | DocView Article
  | EditDocView Article

type Page
  = IndexPage
  | ApiIndexPage
  | GuidesIndexPage
  | DocsPage DocsPage
  | NotFound

type WebData a
  = Loading
  | Error
  | Loaded a

type Msg
  = UrlRequest Nav.Request
  | UrlChange Url
  | ArticleQueryResponse ( Result Db.Error ( List Article ) )
  | SiteInfoResponse ( Result Site.Error SiteInfo )
  -- | AddArticleClicked

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


urlParser : P.Parser ( Route -> a ) a
urlParser =
  P.oneOf
    [ P.map Index P.top
    , P.map Docs ( P.s "Docs" )
    , P.map Api ( P.s "Api" )
    , P.map Guides ( P.s "Guides" )
    , P.map EditDocs ( P.s "Docs" </> P.int )
    ]

parseRoute : Url -> Route
parseRoute u =
  P.parse urlParser u
    |> Maybe.withDefault Index


init : () -> Nav.Key -> Url -> ( Model, Command Msg )
init _ key url =
  (
    { navKey = key
    , route = parseRoute url
    , docs = Loading
    , siteInfo = Loading
    }
  , Command.batch
    [ Db.query
      { query = "SELECT * FROM docs ORDER BY position"
      , params = JE.null
      , expect = Db.expectJson ( JD.list Article.decoder ) ArticleQueryResponse
      }
    , Site.getSiteInfo SiteInfoResponse
    ]
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
      ( { model | route = parseRoute u }, Command.none )
    ArticleQueryResponse res ->
      case res of
        Err err -> ( { model | docs = Error }, Notification.send Notification.Error ( "Failed to load articles: " ++ Db.errorToString err ) Nothing )
        Ok articles -> ( { model | docs = Loaded articles }, Command.none )
    SiteInfoResponse res ->
      case res of
        Err err -> ( { model | siteInfo = Error }, Notification.send Notification.Error "Failed to get site info" Nothing )
        Ok si -> ( { model | siteInfo = Loaded si }, Command.none )

viewNav : Model -> Html Msg
viewNav model =
  nav [ class "navbar" ]
    [ div [ class "container" ]
      [ div [ class "navbar-brand" ]
        [ a [ classList [ ( "navbar-item", True ), ( "is-active", model.route == Index ) ], href "?" ]
          [ img [ src "./logo.png", alt "Elm-ZeroNet" ] []
          ]
        ]
      , div [ class "navbar-menu" ]
        [ div [ class "navbar-end" ]
          [ a [ classList [ ( "navbar-item", True ), ( "is-active", model.route == Docs ) ], href "?Docs" ] [ text "Documentation" ]
          , a [ classList [ ( "navbar-item", True ), ( "is-active", model.route == Api ) ], href "?Api" ] [ text "API Reference" ]
          , a [ classList [ ( "navbar-item", True ), ( "is-active", model.route == Guides ) ], href "?Guides" ] [ text "Guides" ]
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
    , case model.route of
        Index -> viewHero
        Docs -> viewDocs model
        EditDocs _ -> viewDocs model
        Api -> viewApi
        Guides -> viewGuides
    ]

viewDocsMenu : Model -> List ( Html Msg )
viewDocsMenu model =
  case model.docs of
    Loading -> [ text "Loading..." ]
    Error -> [ text "Error, reload?" ]
    Loaded articles -> case articles of
      [] -> [ text "No articles found..." ]
      _ -> articles
        |> List.map (\x ->
          a [ href "#" ] [ text <| Article.title x ]
        )

viewAddDocsButton : WebData SiteInfo -> Html Msg
viewAddDocsButton wdSi =
  let
    showButton =
      case wdSi of
        Loaded si -> si.settings.own
        _ -> False
  in
    if showButton then button [ type_ "button", class "button"  ] [ text "Add article" ]
    else text ""

viewDocs : Model -> Html Msg
viewDocs model =
  section [ class "section" ]
    [ div [ class "container" ]
      [ div [ class "columns" ]
        [ div [ class "column has-text-centered" ]
          [ viewAddDocsButton model.siteInfo
          ]
        ]
      , div [ class "columns" ]
        [ div [class "column is-one-third"] <| viewDocsMenu model
        , div [ class "column" ]
          [ text "Content"
          ]
        ]
      ]
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

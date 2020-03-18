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

import Elm.Docs as Docs exposing ( Module )
import Elm.Type as Type exposing ( Type )

import Markdown

type Route
  = Index
  | Docs
  | EditDocs Int
  | Api
  | ViewApi String
  | Guides

type alias Model =
  { navKey : Nav.Key
  , route : Route
  , docs : WebData ( List Article )
  , siteInfo : WebData SiteInfo
  , apiDocs : WebData ( List Module )
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
  | ApiDocsResponse ( Result Files.Error ( List Module ) )
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
    , P.map ViewApi ( P.s "Api" </> P.string )
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
    , apiDocs = Loading
    }
  , Command.batch
    [ Files.get
      { path = "data/docs.json"
      , required = True
      , timeout = Nothing
      , expect = Files.expectJson ( JD.list Docs.decoder ) ApiDocsResponse
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
    ApiDocsResponse res ->
      case res of
        Err err -> ( { model | apiDocs = Error }, Notification.send Notification.Error "Failed to load api reference" Nothing )
        Ok docs -> ( { model | apiDocs = Loaded docs }, Command.none )

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
    , div [ class "hero-foot" ]
      [ nav [ class "tabs" ]
        [ div [ class "container" ]
          [ ul []
            [ li [] [ a [ href "/19ZNbKNRKFfL5nQHRs6h5L1zsoic87u55A" ] [ text "GitCenter" ] ]
            , li [] [ a [ href "https://github.com/GomZik/elm-zeronet" ] [ text "GitHub" ] ]
            ]
          ]
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
        Api -> viewApi model Nothing
        ViewApi topic -> viewApi model <| Just topic
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

viewApi : Model -> Maybe String -> Html Msg
viewApi model mbTopic =
  case model.apiDocs of
    Loaded docs -> section [ class "section" ]
      [ div [ class "container" ]
        [ div [ class "columns" ]
          [ div [ class "column is-one-third" ]
            ( docs
              |> List.map (\x -> p [] [ a [ href ( "?Api/" ++ x.name ) ] [ text x.name ] ]) )
          , div [ class "column" ]
            [ mbTopic
              |> Maybe.andThen (\topic ->
                docs
                  |> List.filter (\x -> x.name == topic)
                  |> List.head
              )
              |> Maybe.map viewApiTopic
              |> Maybe.withDefault ( text "Select an topic from left side pannel" )
            ]
          ]
        ]
      ]
    _ -> progress [ class "progress" ] []

nbsp : String
nbsp = String.fromChar <| Char.fromCode 8239

viewBlock : Docs.Block -> Html Msg
viewBlock blk =
  case blk of
    Docs.MarkdownBlock md ->
      viewMarkdown md
    Docs.ValueBlock val ->
      viewValue val
    Docs.UnionBlock uni ->
      viewUnion uni
    Docs.AliasBlock als ->
      viewAlias als
    _ -> text "UNEXPECTED BLOCK DETECTED"


viewUnion : Docs.Union -> Html Msg
viewUnion uni =
  let
    renderedUni =
      case uni.tags of
        [] -> p [ class "is-family-monospace" ] <| [ text <| "type " ++ uni.name ++ " " ++ String.join " " uni.args ]
        ( name, types ) :: xs ->
          div []
            <| ( p [ class "is-family-monospace" ]
              <| [ text <| "type " ++ uni.name ++ " " ++ String.join " " uni.args
                ++ if List.length uni.tags == 1 then " = "
                else ""
                ]
            ) :: if List.length uni.tags > 1 then
                ( p [ class "is-family-monospace" ] <| ( text <| String.repeat 4 nbsp  ++ " = " ) :: text name :: List.concatMap ( viewTipe 8 Type ) types
                ) :: ( xs
                    |> List.map (\(cname, ctypes) ->
                      p [ class "is-family-monospace" ] <| ( text <| String.repeat 4 nbsp ++ " | " ++ cname ) :: List.concatMap ( viewTipe 8 Type ) ctypes
                    ) )
              else text name :: List.concatMap ( viewTipe 4 Type ) types
  in
    div [] [ renderedUni, viewMarkdown uni.comment ]

viewAlias : Docs.Alias -> Html Msg
viewAlias als =
  div []
    [ p [ class "is-family-monospace" ] <|
      [ text <| "type alias " ++ als.name ++ " "
      , text <| String.join " " als.args
      , text " = "
      ]
    , p [ class "is-family-monospace" ]
      <| viewTipe 4 Alias als.tipe
    , viewMarkdown als.comment
    ]


viewValue : Docs.Value -> Html Msg
viewValue val =
  div []
    [ p [ class "is-family-monospace" ] ( text ( val.name ++ " : " )
    :: viewTipe 0 TopLevel val.tipe )
    , viewMarkdown val.comment
    ]

viewMarkdown : String -> Html Msg
viewMarkdown = Markdown.toHtml [ class "content" ]

type Context = TopLevel | Lambda | Type | Alias

viewTipe : Int -> Context -> Type -> List ( Html Msg )
viewTipe indent context tp =
  case tp of
    Type.Var name -> [ text <| " " ++ name ++ " " ]
    Type.Lambda inargs outargs ->
        text ( String.repeat indent nbsp )
        :: ( if context == Lambda then text "( "
            else text "" )
        :: viewTipe indent Lambda inargs
        ++ text " -> "
        :: viewTipe indent context outargs
        ++ [ if context == Lambda then text " )"
            else text ""
          ]
    Type.Type name tps ->
      let
        parts = String.split "." name
        mdl = parts
          |> List.head
          |> Maybe.withDefault name
        tpName = parts
          |> List.reverse
          |> List.head
          |> Maybe.withDefault name

        topicUrl = "?Api/" ++ ( parts
          |> List.reverse
          |> List.drop 1
          |> List.filter ( (/=) "Internal" )
          |> List.reverse
          |> String.join "." ) ++ "#" ++ tpName

        isCurrentModule = mdl == "ZeroNet"
      in
        ( if context == Type && List.length tps > 0 then text "( "
          else if context == Alias then text <| String.repeat indent nbsp
          else text "" )
        :: ( if isCurrentModule then a [ href topicUrl ] [ text ( " " ++ tpName ++ " " ) ]
             else text ( " " ++ tpName ++ " " )
           )
        :: List.concatMap ( viewTipe indent Type ) tps
        ++ [ if context == Type && List.length tps > 0 then text " )"
            else text ""
          ]
    Type.Record [] Nothing -> [ text "{}" ]
    Type.Record ( x :: xs ) extension ->
      ( if context == Type then p [] []
        else text "" )
      :: text ( String.repeat indent nbsp )
      :: text
        ( "{ "
        ++ ( case extension of
              Just ext -> ext ++ " | "
              Nothing -> ""
           )
        ++ Tuple.first x
        ++ " : "
        )
      :: ( viewTipe 0 TopLevel <| Tuple.second x )
      ++ ( xs
            |> List.map (\(name, ftp) ->
              p [] <| text ( String.repeat indent nbsp ++ ", " ++ name ++ " : " ) :: viewTipe 0 TopLevel ftp
            )
         )
      ++ [ p [] [ text <| String.repeat indent nbsp ++ "}" ] ]
    Type.Tuple [] -> [ text "()" ]
    Type.Tuple tps ->
      text "( "
      :: (
        List.concatMap (\x -> viewTipe 0 context x ++ [ text ", " ] ) tps
          |> List.reverse
          |> List.drop 1
          |> List.reverse
      )
      ++ [ text " )" ]

    _ -> [ text "SomeOther" ]


viewApiTopic : Module -> Html Msg
viewApiTopic doc =
  let
    blocks = Docs.toBlocks doc
  in
    div []
      ( h1 [ class "title" ] [ text doc.name ]
      :: ( blocks
          |> List.concatMap (\x -> [ viewBlock x, hr [] [] ])
         )
      )

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

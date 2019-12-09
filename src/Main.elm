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

import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Html.Events exposing ( .. )

import Url exposing ( Url )
import Url.Parser as P

import Json.Encode as JE
import Json.Decode as JD

import Time

import Task

type ExternalType = Zite String | Clearnet String

type Page = Page1 | Page2 | External ExternalType

type alias Message =
  { id : Int
  , msg : String
  , created : Time.Posix
  }

type alias ChatData =
  { nextMsgId : Int
  , messages : List Message
  }

timeDecoder : JD.Decoder Time.Posix
timeDecoder =
  JD.int
    |> JD.andThen ( \x -> JD.succeed << Time.millisToPosix <| x * 1000 )

encodeTime : Time.Posix -> JE.Value
encodeTime t =
  t
    |> Time.posixToMillis
    |> (\x -> x // 1000)
    |> JE.int

messageDecoder : JD.Decoder Message
messageDecoder =
  JD.map3 Message
    ( JD.field "id" JD.int )
    ( JD.field "msg" JD.string )
    ( JD.field "created" timeDecoder )

chatDataDecoder : JD.Decoder ChatData
chatDataDecoder =
  JD.map2 ChatData
    ( JD.field "next_msg_id" JD.int )
    ( JD.field "message" <| JD.list messageDecoder )

encodeMessage : Message -> JE.Value
encodeMessage m =
  JE.object
    [ ( "id", JE.int m.id )
    , ( "msg", JE.string m.msg )
    , ( "created", encodeTime m.created )
    ]

encodeChatData : ChatData -> JE.Value
encodeChatData cd =
  JE.object
    [ ( "next_msg_id", JE.int cd.nextMsgId )
    , ( "message", JE.list encodeMessage cd.messages )
    ]

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
  , auth : Maybe User
  , fileContents : String
  , myChatData : ChatData
  , draftMessage : String
  , lastMessages : List ChatMessage
  }

type Msg
  = Inc
  | Dec
  | UrlRequest Nav.Request
  | UrlChange Url
  | LoadExternal ExternalType
  | Login Auth.CertDomains
  | CertChanged ( Maybe User )
  | GotSiteInfo ( Result Site.Error SiteInfo )
  | GotMyChatData ( Result Files.Error ChatData )
  | ChatMessageUpdate String
  | PrepareMessage
  | MsgPrepared ( Int -> Message )
  | MsgSended ( Result Files.Error () )
  | MsgPublished ( Result Site.Error () )
  | GotMessages ( Result Db.Error ( List ChatMessage ) )
  | QueryMessages

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

type alias ChatMessage =
  { msg : String
  , id : String
  }

chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
  JD.map2 ChatMessage
    ( JD.field "msg" JD.string )
    ( JD.field "user_id" JD.string )

init : () -> Nav.Key -> Url -> ( Model, Command Msg )
init _ key url =
  ( { counter = 0
    , page = parseUrl url
    , key = key
    , auth = Nothing
    , fileContents = ""
    , myChatData =
      { nextMsgId = 1
      , messages = []
      }
    , draftMessage = ""
    , lastMessages = []
    }
  , Command.batch
    [ Site.getSiteInfo GotSiteInfo
    , loadMessages
    ]
  )

loadMessages : Command Msg
loadMessages = Db.query
  { query = "SELECT json.cert_user_id as user_id, message.msg as msg FROM message LEFT JOIN json ON message.json_id = json.json_id ORDER BY created DESC LIMIT 20"
  , params = JE.null
  , expect = Db.expectJson ( JD.list chatMessageDecoder ) GotMessages
  }

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
    GotSiteInfo res ->
      case res of
        Err _ -> ( model, Command.none )
        Ok si ->
          let
            mbUser = User.fromSiteInfo si
          in
            ( { model | auth = mbUser }
            , mbUser
              |> Maybe.map (\user ->
                Files.get
                  { path = "data/users/" ++ user.certAddress ++ "/data.json"
                  , expect = Files.expectJson chatDataDecoder GotMyChatData
                  , required = True
                  , timeout = Nothing
                  }
              )
              |> Maybe.withDefault Command.none
            )
    GotMyChatData res ->
      case res of
        Err _ -> ( model, Command.none )
        Ok cd -> ( { model | myChatData = cd }, Command.none )
    ChatMessageUpdate str ->
      ( { model | draftMessage = str }, Command.none )
    PrepareMessage ->
      let
        task = Time.now
          |> Task.andThen (\now ->
            Task.succeed <| (\id -> Message id model.draftMessage now )
          )
      in
        ( model, Command.cmd <| Task.perform MsgPrepared task )
    MsgPrepared toMessage ->
      case model.auth of
        Nothing -> ( model, Command.none )
        Just user ->
          let
            chatData = model.myChatData
            message = toMessage <| chatData.nextMsgId
            newChatData = { chatData | nextMsgId = chatData.nextMsgId + 1, messages = message :: chatData.messages }
          in
            ( { model | myChatData = newChatData, draftMessage = "" }
            , Files.put MsgSended
              ( "data/users/" ++ user.certAddress ++ "/data.json" )
              ( Files.Json <| encodeChatData newChatData )
            )
    QueryMessages ->
      ( model
      , Db.query
        { query = "SELECT json.cert_user_id as user_id, message.msg as msg FROM message LEFT JOIN json ON message.json_id = json.json_id ORDER BY created LIMIT 20"
        , params = JE.null
        , expect = Db.expectJson ( JD.fail "test" ) ( always Inc )
        }
      )
    MsgSended res ->
      case res of
        Err _ -> ( model, Command.none )
        Ok _ -> model.auth
          |> Maybe.map (\user ->
            ( model, Site.publish MsgPublished ( "data/users/" ++ user.certAddress ++ "/data.json" ) )
          )
          |> Maybe.withDefault ( model, Command.none )
    MsgPublished _ ->
      ( model, loadMessages )
    GotMessages res ->
      case res of
        Err _ -> ( model, Command.none )
        Ok msgs -> ( { model | lastMessages = msgs }, Command.none )
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
        Just user -> p [] [ text <| "Hello, " ++ user.certName ]
    , button [ type_ "button", onClick <| Login Auth.Any ] [ text "Login (ANY)" ]
    , button [ type_ "button", onClick <| Login <| Auth.Filter { pattern = Nothing, domains = [ "zeroid.bit" ] } ] [ text "Login (ZeroID only)" ]
    ]


viewChatExample : Model -> Html Msg
viewChatExample model =
  div []
    [ h1 [] [ text "Chat" ]
    , case model.auth of
      Nothing -> text "You need to login to write message"
      Just _ ->
        div []
          [ Html.form [ onSubmit PrepareMessage ]
            [ input [ type_ "text", placeholder "Enter message", value model.draftMessage, onInput <| ChatMessageUpdate ] []
            , button [ type_ "submit" ] [ text "Send" ]
            ]
          ]
    , div [] <|
      h2 [] [ text "messages" ] :: List.map (\x -> p [] [ text <| x.id ++ ": " ++ x.msg ]) model.lastMessages
    ]


view : Model -> Html Msg
view model =
  div []
    [ viewCounterExample model
    , viewRouterExample model
    , viewLoginExample model
    , viewChatExample model
    ]

subscriptions : Model -> Subscription Msg
subscriptions _ =
  Auth.certChange CertChanged

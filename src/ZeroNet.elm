port module ZeroNet exposing ( Program, program )

import Html exposing ( Html )
import Browser exposing ( Document )
import Browser.Navigation as Nav
import Url exposing ( Url )
import ZeroNet.Command.Internal as CmdI exposing ( Command )
import ZeroNet.Navigation exposing ( Request(..) )
import ZeroNet.Navigation.Internal exposing ( Key(..) )
import Json.Encode as JE exposing ( Value )


port zfSend : Value -> Cmd msg


runCmd : Command msg -> Model model -> ( Model model, Cmd ( Msg msg ) )
runCmd cmd model =
  case cmd of
    CmdI.None -> ( model, Cmd.none )
    CmdI.ZFrame zframeCmd args ->
      ( model
      , zfSend <| JE.object
        [ ( "command", JE.string zframeCmd )
        , ( "args", args )
        ]
      )

type Msg msg
  = NoOp
  | AppMsg msg
  | UrlRequest Browser.UrlRequest
  | UrlChange Url

type alias Model model =
  { appModel : model
  , origin : Url
  }

type alias Flags flags =
  { appFlags : flags
  }

type alias Program flags model msg =
  Platform.Program ( Flags flags ) ( Model model ) ( Msg msg )

type alias ProgramConfig flags model msg =
  { init : flags -> Key -> ( model, Command msg )
  , update : msg -> model -> ( model, Command msg )
  , view : model -> Html msg
  , onUrlRequest : Request -> msg
  , onUrlChange : Url -> msg
  }

wrapInit : ( flags -> Key -> ( model, Command msg ) ) -> Flags flags -> Url -> Nav.Key -> ( Model model, Cmd ( Msg msg ) )
wrapInit fn flags origin _ =
  let
    ( internalModel, cmd ) = fn flags.appFlags Key
    model =
      { appModel = internalModel
      , origin = origin
      }
  in
    runCmd cmd model

wrapUpdate : ProgramConfig flags model msg -> Msg msg -> Model model -> ( Model model, Cmd ( Msg msg ) )
wrapUpdate cfg msg model =
  -- case Debug.log "Message: " msg of
  case msg of
    AppMsg m ->
      let
        ( newAppModel, cmd ) = cfg.update m model.appModel
        newModel = { model | appModel = newAppModel }
      in
        runCmd cmd newModel
    UrlChange _ -> ( model, Cmd.none )
    UrlRequest req ->
      let
        znReq = mapRequest model.origin req
        appMsg = cfg.onUrlRequest znReq
        ( newAppModel, cmd ) = cfg.update appMsg model.appModel
        newModel = { model | appModel = newAppModel }
      in
        runCmd cmd newModel
    NoOp ->
      ( model, Cmd.none )


wrapView : ( model -> Html msg ) -> Model model -> Document ( Msg msg )
wrapView fn model =
  { title = "ZeroNet"
  , body = fn model.appModel
    |> Html.map AppMsg
    |> List.singleton
  }


mapRequest : Url -> Browser.UrlRequest -> Request
mapRequest origin req =
  case req of
    Browser.External str -> External str
    Browser.Internal u ->
      if u.path == origin.path then
        Internal
          { host = u.path
          , port_ = u.port_
          , path = Maybe.withDefault "" u.query
          , query = Nothing
          , fragment = u.fragment
          , protocol = u.protocol
          }
      else Zite <| Url.toString u


program : ProgramConfig flags model msg -> Program flags model msg
program cfg =
  Browser.application
    { subscriptions = always Sub.none
    , init = wrapInit cfg.init
    , update = wrapUpdate cfg
    , view = wrapView cfg.view
    , onUrlChange = UrlChange
    , onUrlRequest = UrlRequest
    }

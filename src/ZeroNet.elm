port module ZeroNet exposing ( Program, program )

import Html exposing ( Html )
import Browser exposing ( Document )
import Browser.Navigation as Nav
import Url exposing ( Url )

import ZeroNet.Command.Internal as CmdI exposing ( Command )
import ZeroNet.Subscription.Internal as SubI exposing ( Subscription )
import ZeroNet.Navigation exposing ( Request(..) )
import ZeroNet.Navigation.Internal exposing ( Key(..) )
import ZeroNet.SiteInfo as SiteInfo exposing ( SiteInfo )

import Json.Encode as JE exposing ( Value )
import Json.Decode as JD

import Dict exposing ( Dict )


port zfSend : Value -> Cmd msg
port zfResponse : ( Value -> msg ) -> Sub msg
port urlChanged : ( String -> msg ) -> Sub msg
port siteInfoChanged : ( Value -> msg ) -> Sub msg

insertToDict : x -> Dict Int x -> ( Int, Dict Int x )
insertToDict val d =
  let
    id = d
      |> Dict.keys
      |> List.reverse
      |> List.head
      |> Maybe.withDefault 0
      |> (+) 1
  in
    ( id, Dict.insert id val d )

runCmd : Command msg -> Model model msg -> ( Model model msg, Cmd ( Msg msg ) )
runCmd cmd model =
  case cmd of
    CmdI.None -> ( model, Cmd.none )
    CmdI.ZFrame zframeCmd args cb ->
      case cb of
        CmdI.NoResponse ->
          ( model
          , zfSend <| JE.object
            [ ( "command", JE.string zframeCmd )
            , ( "args", args )
            ]
          )
        CmdI.Response fn ->
          let
            ( id, newCallbacks ) = insertToDict fn model.zframeCallbacks
          in
            ( { model | zframeCallbacks = newCallbacks }
            , zfSend <| JE.object
              [ ( "command", JE.string zframeCmd )
              , ( "args", args )
              , ( "reqId", JE.int id )
              ]
            )

    CmdI.Platform c -> ( model, Cmd.map AppMsg c )
    CmdI.Batch cmds ->
      let
        helper : List ( Command msg ) -> Model model msg -> List ( Cmd ( Msg msg ) ) -> ( Model model msg, Cmd ( Msg msg ) )
        helper commandsStep modelStep acc =
          case commandsStep of
            step :: rest ->
              let
                ( newModel, resultCmd ) = runCmd step modelStep
              in
                helper rest newModel ( resultCmd :: acc )
            [] ->
              ( model, Cmd.batch <| List.reverse acc )
      in
        helper cmds model []

type Msg msg
  = NoOp
  | AppMsg msg
  | UrlRequest Browser.UrlRequest
  | UrlChange Url
  | IframeUrlchanged String
  | SendUserCertToApp ( Maybe String -> msg ) Value
  | GotZfResponse Value

type alias Model model msg =
  { appModel : model
  , origin : Url
  , siteInfo : Maybe SiteInfo
  , zframeCallbacks : Dict Int ( Result CmdI.ZFrameError Value -> msg )
  }

type alias Flags flags =
  { appFlags : flags
  }

type alias Program flags model msg =
  Platform.Program ( Flags flags ) ( Model model msg ) ( Msg msg )

type alias ProgramConfig flags model msg =
  { init : flags -> Key -> Url -> ( model, Command msg )
  , update : msg -> model -> ( model, Command msg )
  , view : model -> Html msg
  , subscriptions : model -> Subscription msg
  , onUrlRequest : Request -> msg
  , onUrlChange : Url -> msg
  }

cutWrapperNonce : String -> String
cutWrapperNonce =
  String.split "&"
  >> List.filter ( not << String.startsWith "wrapper_nonce=" )
  >> String.join "&"

wrapInit : ( flags -> Key -> Url -> ( model, Command msg ) ) -> Flags flags -> Url -> Nav.Key -> ( Model model msg, Cmd ( Msg msg ) )
wrapInit fn flags origin _ =
  let
    ( internalModel, cmd ) =
      fn flags.appFlags Key
        { origin | host = origin.path,
                   query = Nothing,
                   path = origin.query
                    |> Maybe.map cutWrapperNonce
                    |> Maybe.withDefault ""
        }
    model =
      { appModel = internalModel
      , origin = origin
      , siteInfo = Nothing
      , zframeCallbacks = Dict.empty
      }
  in
    runCmd cmd model

wrapUpdate : ProgramConfig flags model msg -> Msg msg -> Model model msg -> ( Model model msg, Cmd ( Msg msg ) )
wrapUpdate cfg msg model =
  -- case Debug.log "Message: " msg of
  case msg of
    AppMsg m ->
      let
        ( newAppModel, cmd ) = cfg.update m model.appModel
        newModel = { model | appModel = newAppModel }
      in
        runCmd cmd newModel
    -- UrlChange does not fires in Iframe, because not iframe changes url
    UrlChange _ -> ( model, Cmd.none )
    UrlRequest req ->
      let
        znReq = mapRequest model.origin req
        appMsg = cfg.onUrlRequest znReq
        ( newAppModel, cmd ) = cfg.update appMsg model.appModel
        newModel = { model | appModel = newAppModel }
      in
        runCmd cmd newModel
    IframeUrlchanged newUrlStr ->
      let
        origin = model.origin
        newUrl = { origin | host = origin.path, path = newUrlStr, query = Nothing }

        ( newAppModel, cmd ) = cfg.update ( cfg.onUrlChange newUrl ) model.appModel
        newModel = { model | appModel = newAppModel }
      in
        runCmd cmd newModel
    SendUserCertToApp cb val ->
      case JD.decodeValue SiteInfo.decoder val of
        Ok si ->
          let
            ( newAppModel, cmd ) = cfg.update ( cb si.certUserId ) model.appModel
            newModel = { model | siteInfo = Just si, appModel = newAppModel }
          in
            runCmd cmd newModel
        Err _ -> ( model, Cmd.none )
    GotZfResponse val ->
      case JD.decodeValue zfResponseDecoder val of
        Err err -> ( model, Cmd.none )
        Ok resp ->
          case Dict.get resp.id model.zframeCallbacks of
            Nothing ->( model, Cmd.none )
            Just toMsg ->
              let
                ( newAppModel, cmd ) = cfg.update ( toMsg <| Ok resp.value ) model.appModel
                newCallbacks = Dict.remove resp.id model.zframeCallbacks
              in
                runCmd cmd { model | zframeCallbacks = newCallbacks, appModel = newAppModel }
    NoOp ->
      ( model, Cmd.none )

type alias ZFResponse =
  { id : Int
  , value : JD.Value
  }

zfResponseDecoder : JD.Decoder ZFResponse
zfResponseDecoder =
  JD.map2 ZFResponse
    ( JD.field "id" JD.int )
    ( JD.field "response" JD.value )


wrapView : ( model -> Html msg ) -> Model model msg -> Document ( Msg msg )
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
          , path = u.query
            |> Maybe.map (\x -> if x == "" then x else "/" ++ x)
            |> Maybe.withDefault ""
          , query = Nothing
          , fragment = u.fragment
          , protocol = u.protocol
          }
      else Zite <| Url.toString u


runSubs : Subscription msg -> Model model msg -> Sub ( Msg msg )
runSubs sub model =
  case sub of
    SubI.CertChange cb ->
      siteInfoChanged <| SendUserCertToApp cb
    SubI.Batch subs ->
      Sub.batch <|
        List.map ( \x -> runSubs x model ) subs
    SubI.None ->
      Sub.none


wrapSubscriptions : ( model -> Subscription msg ) -> Model model msg -> Sub ( Msg msg )
wrapSubscriptions fn model =
  let
    appSubs = fn model.appModel
    subs = runSubs appSubs model
  in
    Sub.batch
      [ urlChanged IframeUrlchanged
      , zfResponse GotZfResponse
      , subs
      ]


program : ProgramConfig flags model msg -> Program flags model msg
program cfg =
  Browser.application
    { subscriptions = wrapSubscriptions cfg.subscriptions
    , init = wrapInit cfg.init
    , update = wrapUpdate cfg
    , view = wrapView cfg.view
    , onUrlChange = UrlChange
    , onUrlRequest = UrlRequest
    }

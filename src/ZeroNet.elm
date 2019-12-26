port module ZeroNet exposing ( Program, program, ProgramConfig )

{-| elm-zeronet is a toolkit to build zeronet zites with elm

This module adds own Program as TEA does but adds compatibility layer for ZeroFrame API

# Program

@docs Program, ProgramConfig, program

-}

import Html exposing ( Html )
import Browser exposing ( Document )
import Browser.Navigation as Nav
import Url exposing ( Url )

import ZeroNet.Command.Internal as CmdI exposing ( Command )
import ZeroNet.Subscription.Internal as SubI exposing ( Subscription )
import ZeroNet.Navigation exposing ( Request(..) )
import ZeroNet.Navigation.Internal exposing ( Key(..) )
import ZeroNet.Site as Site exposing ( SiteInfo )
import ZeroNet.Data.User as User exposing ( User )

import Json.Encode as JE exposing ( Value )
import Json.Decode as JD

import Dict exposing ( Dict )


port zfReady : ( () -> msg ) -> Sub msg
port zfSend : Value -> Cmd msg
port zfResponse : ( Value -> msg ) -> Sub msg
port urlChanged : ( String -> msg ) -> Sub msg
port siteInfoChanged : ( Value -> msg ) -> Sub msg
port certChanged : ( Value -> msg ) -> Sub msg
port onFileWrite : ( () -> msg ) -> Sub msg

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

runCmd : Command msg -> Model flags model msg -> ( Model flags model msg, Cmd ( Msg msg ) )
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
        helper : List ( Command msg ) -> Model flags model msg -> List ( Cmd ( Msg msg ) ) -> ( Model flags model msg, Cmd ( Msg msg ) )
        helper commandsStep modelStep acc =
          case commandsStep of
            step :: rest ->
              let
                ( newModel, resultCmd ) = runCmd step modelStep
              in
                helper rest newModel ( resultCmd :: acc )
            [] ->
              ( modelStep, Cmd.batch <| List.reverse acc )
      in
        helper cmds model []

type Msg msg
  = NoOp
  | ZFrameReady ()
  | AppMsg msg
  | UrlRequest Browser.UrlRequest
  | UrlChange Url
  | IframeUrlchanged String
  | SendUserCertToApp ( Maybe User -> msg ) Value
  | GotZfResponse Value

type AppState model
  = Loading
  | Ready model

type alias Model flags model msg =
  -- { appModel : model
  { appState : AppState model
  , origin : Url
  , siteInfo : Maybe SiteInfo
  , zframeCallbacks : Dict Int ( Result CmdI.ZFrameError Value -> msg )
  , flags : flags
  }

type alias Flags flags =
  { appFlags : flags
  }

{-| Program type mimic to Platform.Program but adds an funtcionality to work with ZeroFrame
-}
type alias Program flags model msg =
  Platform.Program ( Flags flags ) ( Model flags model msg ) ( Msg msg )

{-| ProgramConfig parameters that should be passed to `program` function
-}
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

init : Flags flags -> Url -> Nav.Key -> ( Model flags model msg, Cmd ( Msg msg ) )
init flags origin _ =
  ( { appState = Loading
    , origin = origin
    , siteInfo = Nothing
    , zframeCallbacks = Dict.empty
    , flags = flags.appFlags
    }
  , Cmd.none
  )

wrapUpdate : ProgramConfig flags model msg -> Msg msg -> Model flags model msg -> ( Model flags model msg, Cmd ( Msg msg ) )
wrapUpdate cfg msg model =
  -- case Debug.log "Message: " ( msg, model.appState ) of
  case ( msg, model.appState ) of
    ( ZFrameReady _, Loading ) ->
      let
        origin = model.origin
        urlForApp =
          { origin | host = origin.path
                   , query = Nothing
                   , path = origin.query
                      |> Maybe.map cutWrapperNonce
                      |> Maybe.withDefault ""
          }
        ( initialModel, cmd ) = cfg.init model.flags Key urlForApp
        newModel = { model | appState = Ready initialModel }
      in
        runCmd cmd newModel
    ( AppMsg m, Ready appModel ) ->
      let
        ( newAppModel, cmd ) = cfg.update m appModel
        newModel = { model | appState = Ready newAppModel }
      in
        runCmd cmd newModel
    -- UrlChange does not fires in Iframe, because not iframe changes url
    ( UrlChange _, _ ) -> ( model, Cmd.none )
    ( UrlRequest req, Ready appModel ) ->
      let
        znReq = mapRequest model.origin req
        appMsg = cfg.onUrlRequest znReq
        ( newAppModel, cmd ) = cfg.update appMsg appModel
        newModel = { model | appState = Ready newAppModel }
      in
        runCmd cmd newModel
    ( IframeUrlchanged newUrlStr, Ready appModel ) ->
      let
        origin = model.origin
        newUrl = { origin | host = origin.path, path = newUrlStr, query = Nothing }

        ( newAppModel, cmd ) = cfg.update ( cfg.onUrlChange newUrl ) appModel
        newModel = { model | appState = Ready newAppModel }
      in
        runCmd cmd newModel
    ( SendUserCertToApp cb val, Ready appModel ) ->
      case JD.decodeValue Site.decoder val of
        Ok si ->
          let
            ( newAppModel, cmd ) = cfg.update ( cb <| User.fromSiteInfo si ) appModel
            newModel = { model | siteInfo = Just si, appState = Ready newAppModel }
          in
            runCmd cmd newModel
        Err _ -> ( model, Cmd.none )
    ( GotZfResponse val, Ready appModel ) ->
      case JD.decodeValue zfResponseDecoder val of
        Err err -> ( model, Cmd.none )
        Ok resp ->
          case Dict.get resp.id model.zframeCallbacks of
            Nothing ->( model, Cmd.none )
            Just toMsg ->
              let
                ( newAppModel, cmd ) = cfg.update ( toMsg <| Ok resp.value ) appModel
                newCallbacks = Dict.remove resp.id model.zframeCallbacks
              in
                runCmd cmd { model | zframeCallbacks = newCallbacks, appState = Ready newAppModel }
    _ ->
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


wrapView : ( model -> Html msg ) -> Model flags model msg -> Document ( Msg msg )
wrapView fn model =
  case model.appState of
    Loading ->
      { title = "Loading..."
      , body = []
      }
    Ready appModel ->
      { title = "ZeroNet"
      , body = fn appModel
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


runSubs : Subscription msg -> Model flags model msg -> Sub ( Msg msg )
runSubs sub model =
  case sub of
    SubI.CertChange cb ->
      certChanged <| SendUserCertToApp cb
    SubI.Batch subs ->
      Sub.batch <|
        List.map ( \x -> runSubs x model ) subs
    SubI.OnFileWrite msg ->
      onFileWrite <| always <| AppMsg msg
    SubI.None ->
      Sub.none


wrapSubscriptions : ( model -> Subscription msg ) -> Model flags model msg -> Sub ( Msg msg )
wrapSubscriptions fn model =
  case model.appState of
    Loading -> zfReady ZFrameReady
    Ready appModel ->
      let
        appSubs = fn appModel
        subs = runSubs appSubs model
      in
        Sub.batch
          [ urlChanged IframeUrlchanged
          , zfResponse GotZfResponse
          , siteInfoChanged <| always NoOp
          , subs
          ]


{-| program creates an ZeroNet program

    module Main exposing ( main )

    import ZeroNet
    import ZeroNet.Navigation as Nav
    import ZeroNet.Command as Command exposing ( Command )
    import ZeroNet.Subscription as Subscription exposing ( Subscription )

    import Html exposing ( .. )
    import Html.Attributes exposing ( .. )
    import Html.Events exposing ( .. )

    import Url exposing ( Url )

    type alias Model = Int

    type Msg
      = Inc
      | Dec
      | UrlRequest Nav.Request
      | UrlChange Url

    main : ZeroNet.Program Flags Model Msg
    main =
      ZeroNet.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }

    init : () -> Nav.Key -> Url -> ( Model, Command Msg )
    init _ _ _ =
      ( 0, Command.none )

    update : Msg -> Model -> ( Model, Command Msg )
    update =
      case msg of
        Inc ->
          ( model + 1, Command.none )
        Dec ->
          ( model - 1, Command.none )
        _ -> ( model, Command.none )

    view : Model -> Html Msg
    view model =
      div []
        [ button [ type_ "button", onClick Dec ] [ "-" ]
        , span [] [ text <| String.fromInt model ]
        , button [ type_ "button", onClick Inc ] [ "+" ]
        ]

    subscriptions : Model -> Subscription Msg
    subscriptions =
      Sub.none
-}
program : ProgramConfig flags model msg -> Program flags model msg
program cfg =
  Browser.application
    { subscriptions = wrapSubscriptions cfg.subscriptions
    , init = init
    , update = wrapUpdate cfg
    , view = wrapView cfg.view
    , onUrlChange = UrlChange
    , onUrlRequest = UrlRequest
    }

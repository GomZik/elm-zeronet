module ZeroNet exposing ( Program, program, Key )

import Html exposing ( Html )
import Browser exposing ( Document )
import ZeroNet.Command exposing ( Command )


type Msg msg
  = NoOp
  | AppMsg msg

type Model model = M model

type alias Flags flags =
  { appFlags : flags
  }

type Key = Key

type alias Program flags model msg =
  Platform.Program ( Flags flags ) ( Model model ) ( Msg msg )

type alias ProgramConfig flags model msg =
  { init : flags -> Key -> ( model, Command msg )
  , update : msg -> model -> ( model, Command msg )
  , view : model -> Html msg
  }

wrapInit : ( flags -> Key -> ( model, Command msg ) ) -> Flags flags -> ( Model model, Cmd ( Msg msg ) )
wrapInit fn flags =
  let
    ( internalModel, _ ) = fn flags.appFlags Key
  in
    ( M internalModel
    , Cmd.none
    )

wrapUpdate : ( msg -> model -> ( model, Command msg ) ) -> Msg msg -> Model model -> ( Model model, Cmd ( Msg msg ) )
wrapUpdate fn msg ( M model ) =
  case msg of
    AppMsg m ->
      let
        ( newModel, _ ) = fn m model
      in
        ( M newModel, Cmd.none )
    NoOp ->
      ( M model, Cmd.none )


wrapView : ( model -> Html msg ) -> Model model -> Document ( Msg msg )
wrapView fn ( M model ) =
  { title = "ZeroNet"
  , body = fn model
    |> Html.map AppMsg
    |> List.singleton
  }


program : ProgramConfig flags model msg -> Program flags model msg
program cfg =
  Browser.document
    { subscriptions = always Sub.none
    , init = wrapInit cfg.init
    , update = wrapUpdate cfg.update
    , view = wrapView cfg.view
    }

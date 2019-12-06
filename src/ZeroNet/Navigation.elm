port module ZeroNet.Navigation exposing ( Key, Request(..), pushUrl, load )

import ZeroNet.Navigation.Internal as I
import ZeroNet.Command.Internal as CmdI
import Json.Encode as JE

import Url exposing ( Url )

port goUrl : String -> Cmd msg

type alias Key = I.Key

type Request
  = Internal Url
  | Zite String
  | External String


pushUrl : Key -> String -> CmdI.Command msg
pushUrl _ url =
  CmdI.ZFrame
    "wrapperPushState"
    ( JE.list identity
      [ JE.null
      , JE.string "Title"
      , JE.string url
      ] )
    CmdI.NoResponse

load : String -> CmdI.Command msg
load = CmdI.Platform << goUrl

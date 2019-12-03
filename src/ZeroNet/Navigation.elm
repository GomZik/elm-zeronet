module ZeroNet.Navigation exposing ( Key, Request(..), pushUrl )

import ZeroNet.Navigation.Internal as I
import ZeroNet.Command.Internal as CmdI
import Json.Encode as JE

import Url exposing ( Url )

type alias Key = I.Key

type Request
  = Internal Url
  | Zite String
  | External String


pushUrl : Key -> String -> CmdI.Command msg
pushUrl _ url =
  JE.list identity
    [ JE.null
    , JE.string "Title"
    , JE.string url
    ]
    |> CmdI.ZFrame "wrapperPushState"

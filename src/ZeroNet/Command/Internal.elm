module ZeroNet.Command.Internal exposing (..)

import Json.Encode exposing ( Value )

type ZFrameError = E String

errorToString : ZFrameError -> String
errorToString (E s) = s

type ZFrameResponse msg
  = NoResponse
  | Response ( Result ZFrameError Value -> msg )

type Command msg
  = None
  | ZFrame String Value ( ZFrameResponse msg )
  | Platform ( Cmd msg )
  | Batch ( List ( Command msg ) )

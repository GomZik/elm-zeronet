module ZeroNet.Command.Internal exposing (..)

import Json.Encode exposing ( Value )

type Command msg
  = None
  | ZFrame String Value

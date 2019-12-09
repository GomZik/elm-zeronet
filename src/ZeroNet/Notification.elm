module ZeroNet.Notification exposing (Severity(..), send)

import ZeroNet.Command.Internal as CmdI

import Json.Encode as JE

type Severity
  = Info
  | Done
  | Error

encodeSeverity : Severity -> JE.Value
encodeSeverity sev =
  JE.string <| case sev of
    Info -> "info"
    Done -> "done"
    Error -> "error"

send : Severity -> String -> Maybe Int -> CmdI.Command msg
send sev msg timeout =
  CmdI.ZFrame "wrapperNotification"
    ( JE.list identity
      [ encodeSeverity sev
      , JE.string msg
      , JE.int <| Maybe.withDefault 3000 timeout
      ]
    )
    CmdI.NoResponse

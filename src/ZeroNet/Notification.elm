module ZeroNet.Notification exposing (Severity(..), send)

{-| ZeroNet.Notification allows to send notifications to user trough ZeroFrame API

# Types

@docs Severity

# Commands

@docs send
-}

import ZeroNet.Command.Internal as CmdI

import Json.Encode as JE

{-| Severity represent an type of notification
-}
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

{-| Creates command to ZeroFrame API to send notification.

You can pass notification timeout as third parameter. 3000 millis by default
-}
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

module ZeroNet.Command exposing ( Command, none )

import ZeroNet.Command.Internal as I


type alias Command msg = I.Command msg

none : Command msg
none = I.None

module ZeroNet.Command exposing ( Command, none, batch )

import ZeroNet.Command.Internal as I


type alias Command msg = I.Command msg

none : Command msg
none = I.None

batch : List ( Command msg ) -> Command msg
batch = I.Batch

module ZeroNet.Subscription exposing ( Subscription, none, batch )

import ZeroNet.Subscription.Internal as SubI


type alias Subscription msg = SubI.Subscription msg

none : Subscription msg
none = SubI.None

batch : List ( Subscription msg ) -> Subscription msg
batch = SubI.Batch

module ZeroNet.Subscription exposing ( Subscription, none )

import ZeroNet.Subscription.Internal as SubI


type alias Subscription msg = SubI.Subscription msg

none : Subscription msg
none = SubI.None

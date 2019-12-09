module ZeroNet.Subscription.Internal exposing (..)

import ZeroNet.Data.User as User exposing ( User )


type Subscription msg
  = None
  | CertChange ( Maybe User -> msg )
  | OnFileWrite msg
  | Batch ( List ( Subscription msg ) )

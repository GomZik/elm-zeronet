module ZeroNet.Subscription.Internal exposing (..)


type Subscription msg
  = None
  | CertChange ( Maybe String -> msg )
  | Batch ( List ( Subscription msg ) )

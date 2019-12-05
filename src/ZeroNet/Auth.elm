module ZeroNet.Auth exposing ( .. )

import ZeroNet.Command.Internal as CmdI
import ZeroNet.Subscription.Internal as SubI

import Json.Encode as JE

type CertDomains
  = Any
  | Filter
    { domains : List String
    , pattern : Maybe String
    }

certSelect : CertDomains -> CmdI.Command msg
certSelect cd =
  let
    args = case cd of
      Any -> JE.object
        [ ( "accept_any", JE.bool True )
        ]
      Filter data -> JE.object
        [ ( "accepted_domains", JE.list JE.string data.domains )
        , ( "accepted_pattern"
          , case data.pattern of
            Nothing -> JE.null
            Just str -> JE.string str
          )
        ]
  in
    CmdI.ZFrame "certSelect" args


certChange : ( Maybe String -> msg ) -> SubI.Subscription msg
certChange = SubI.CertChange

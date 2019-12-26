module ZeroNet.Auth exposing ( CertDomains(..), certSelect, certChange )

{-| ZeroNet.Auth module allows to work with ZeroFrame cert selction API

# Commands

@docs CertDomains, certSelect

# Subscriptions

@docs certChange
-}

import ZeroNet.Command.Internal as CmdI
import ZeroNet.Subscription.Internal as SubI
import ZeroNet.Data.User as User exposing ( User )

import Json.Encode as JE

{-| CertDomains allows to specify, which certificate can be selected by user.

See [ZeroFrame.certSelect](https://zeronet.io/docs/site_development/zeroframe_api_reference/#certselect)
-}
type CertDomains
  = Any
  | Filter
    { domains : List String
    , pattern : Maybe String
    }

{-| Shows an ZeroFrame dialog that allows user to select certificate.

Suitable ceritficate can be filtered using CertDomains argument

    case msg of
      LoginButtonClicked ->
        ( model
        , Auth.certSelect
          Filter
            { domains = [ "zeroid.bit" ]
            , pattern = Just "1ZeroiD[0-9]"
            }
        )
-}
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
    CmdI.ZFrame "certSelect" args CmdI.NoResponse


{-| Returns subscription that allows to application know, when user selects new certificate
-}
certChange : ( Maybe User -> msg ) -> SubI.Subscription msg
certChange = SubI.CertChange

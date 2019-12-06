module ZeroNet.Data.User exposing ( .. )

import ZeroNet.SiteInfo exposing ( SiteInfo )

type alias User =
  { certName : String
  , certAddress : String
  }

fromSiteInfo : SiteInfo -> Maybe User
fromSiteInfo si =
  case ( si.certUserId, si.authAddress ) of
    ( Just name, Just address ) -> Just <| User name address
    _ -> Nothing

module ZeroNet.SiteInfo exposing (..)

import Json.Decode as JD


type alias SiteInfo =
  { certUserId : Maybe String
  , authAddress : Maybe String
  }

decoder : JD.Decoder SiteInfo
decoder =
  JD.map2 SiteInfo
    ( JD.field "cert_user_id" <| JD.maybe JD.string )
    ( JD.field "auth_address" <| JD.maybe JD.string )

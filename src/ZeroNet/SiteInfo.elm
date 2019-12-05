module ZeroNet.SiteInfo exposing (..)

import Json.Decode as JD


type alias SiteInfo =
  { certUserId : Maybe String
  }

decoder : JD.Decoder SiteInfo
decoder =
  JD.map SiteInfo
    ( JD.field "cert_user_id" <| JD.maybe JD.string )

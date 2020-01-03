module ZeroNet.Site exposing ( SiteInfo, Settings, decoder, getSiteInfo, publish, Error )

{-| ZeroNet.Site represents siteInfo ZeroFrame API methods

# Types

@docs Error, SiteInfo, Settings

# Commands

@docs getSiteInfo, publish

# TODO

@docs decoder
-}

import ZeroNet.Command.Internal as CmdI

import Json.Encode as JE
import Json.Decode as JD


{-| Represents an error while requesting SiteInfo
-}
type Error
  = ZFrameError CmdI.ZFrameError
  | JsonError JD.Error
  | PublishError String

{-| SiteInfo has Settings part
-}
type alias Settings =
  { own : Bool
  }

{-| SiteInfo
-}
type alias SiteInfo =
  { certUserId : Maybe String
  , authAddress : Maybe String
  , settings: Settings
  }

settingsDecoder : JD.Decoder Settings
settingsDecoder =
  JD.map Settings
    ( JD.field "own" JD.bool )

{-| TODO: Make internal
-}
decoder : JD.Decoder SiteInfo
decoder =
  JD.map3 SiteInfo
    ( JD.field "cert_user_id" <| JD.maybe JD.string )
    ( JD.field "auth_address" <| JD.maybe JD.string )
    ( JD.field "settings" <| settingsDecoder )


{-| Returns command to request siteInfo
-}
getSiteInfo : ( Result Error SiteInfo -> msg ) -> CmdI.Command msg
getSiteInfo cb =
  CmdI.ZFrame "siteInfo" JE.null <| CmdI.Response <| ( \res ->
    case res of
      Ok val -> case JD.decodeValue decoder val of
        Ok si -> cb <| Ok si
        Err err -> cb <| Err <| JsonError err
      Err err -> cb <| Err <| ZFrameError err
  )

publishResponseDecoder : JD.Decoder ( Result Error () )
publishResponseDecoder =
  JD.oneOf
    [ JD.field "error" JD.string
      |> JD.andThen (\str -> JD.succeed <| Err <| PublishError str )
    , JD.succeed <| Ok ()
    ]

{-| Returns command to publish and subscribe spefified file
-}
publish : ( Result Error () -> msg ) -> String -> CmdI.Command msg
publish cb path =
  CmdI.ZFrame "sitePublish"
    ( JE.object
      [ ( "inner_path", JE.string path )
      ]
    )
    ( CmdI.Response (\res ->
      case res of
        Err err -> cb <| Err <| ZFrameError err
        Ok val -> case JD.decodeValue publishResponseDecoder val of
          Ok decres -> cb decres
          Err err -> cb <| Err <| JsonError err
    ) )

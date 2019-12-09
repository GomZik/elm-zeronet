module ZeroNet.Site exposing ( SiteInfo, decoder, getSiteInfo, publish, Error )

import ZeroNet.Command.Internal as CmdI

import Json.Encode as JE
import Json.Decode as JD


type Error
  = ZFrameError CmdI.ZFrameError
  | JsonError JD.Error
  | PublishError String


type alias SiteInfo =
  { certUserId : Maybe String
  , authAddress : Maybe String
  }

decoder : JD.Decoder SiteInfo
decoder =
  JD.map2 SiteInfo
    ( JD.field "cert_user_id" <| JD.maybe JD.string )
    ( JD.field "auth_address" <| JD.maybe JD.string )


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

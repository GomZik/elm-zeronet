module ZeroNet.Database exposing
  ( Error, Expect, expectJson, query )

import ZeroNet.Command.Internal as CmdI
import Json.Encode as JE exposing ( Value )
import Json.Decode as JD exposing ( Decoder )


type Error
  = JsonParseError JD.Error
  | RequestError CmdI.ZFrameError

type Expect tp msg = Json ( Decoder tp ) ( Result Error tp -> msg )

type alias QueryParams tp msg =
  { query : String
  , params : Value
  , expect : Expect tp msg
  }

expectJson : Decoder tp -> ( Result Error tp -> msg ) -> Expect tp msg
expectJson = Json

processResponse : Expect tp msg -> Result CmdI.ZFrameError Value -> msg
processResponse ( Json dec cb ) res =
  let
    expectResult = case res of
      Err err -> Err <| RequestError err
      Ok val ->
        case JD.decodeValue dec val of
          Err err -> Err <| JsonParseError err
          Ok tp ->
            Ok tp
  in
    cb expectResult

query : QueryParams tp msg -> CmdI.Command msg
query prms =
  CmdI.ZFrame "dbQuery"
    ( JE.list identity <|
      [ JE.string prms.query
      , prms.params
      ] )
    ( CmdI.Response <| processResponse prms.expect )

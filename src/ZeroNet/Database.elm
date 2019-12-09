module ZeroNet.Database exposing
  ( Error, Expect, expectJson, query, expectValue )

import ZeroNet.Command.Internal as CmdI
import Json.Encode as JE exposing ( Value )
import Json.Decode as JD exposing ( Decoder )


type Error
  = JsonParseError JD.Error
  | RequestError CmdI.ZFrameError

type Expect msg = Val ( Result Error Value -> msg )

type alias QueryParams msg =
  { query : String
  , params : Value
  , expect : Expect msg
  }

expectJson : Decoder tp -> ( Result Error tp -> msg ) -> Expect msg
expectJson dec cb =
  Val (\res ->
    case res of
      Err err -> cb <| Err err
      Ok val -> case JD.decodeValue dec val of
        Err err -> cb <| Err <| JsonParseError err
        Ok tp -> cb <| Ok tp
  )

expectValue : ( Result Error Value -> msg ) -> Expect msg
expectValue = Val

processResponse : Expect msg -> Result CmdI.ZFrameError Value -> msg
processResponse ( Val cb ) res =
  res
    |> Result.mapError RequestError
    |> cb

query : QueryParams msg -> CmdI.Command msg
query prms =
  CmdI.ZFrame "dbQuery"
    ( JE.list identity <|
      [ JE.string prms.query
      , prms.params
      ] )
    ( CmdI.Response <| processResponse prms.expect )

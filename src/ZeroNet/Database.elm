module ZeroNet.Database exposing
  ( Error, Expect, expectJson, query, expectValue
  , errorToString, QueryParams
  )

{-| ZeroNet.Database module allows to work with database.

You need a proper dbschema.json file first,
see [Structure of dbschema.json](https://zeronet.io/docs/site_development/dbschema_json/)

# Types

@docs Error, errorToString

# Queries

@docs QueryParams, query, Expect, expectValue, expectJson
-}

import ZeroNet.Command.Internal as CmdI
import Json.Encode as JE exposing ( Value )
import Json.Decode as JD exposing ( Decoder )


{-| Error represents an error, that can be happened while db query
-}
type Error
  = JsonParseError JD.Error
  | RequestError CmdI.ZFrameError

{-| errorToString transforms Error type to string
-}
errorToString : Error -> String
errorToString err =
  case err of
    JsonParseError jderr -> "failed to parse json: " ++ JD.errorToString jderr
    RequestError zferr -> "failed to query data: " ++ CmdI.errorToString zferr

{-| Expect type tells ZeroNet runtime, what we expect as a result of query
-}
type Expect msg = Val ( Result Error Value -> msg )

{-| QueryParams is just alias for params of a query command
-}
type alias QueryParams msg =
  { query : String
  , params : Value
  , expect : Expect msg
  }

{-| expectJson tells ZeroNet runtime that we expect json decoded type
-}
expectJson : Decoder tp -> ( Result Error tp -> msg ) -> Expect msg
expectJson dec cb =
  Val (\res ->
    case res of
      Err err -> cb <| Err err
      Ok val -> case JD.decodeValue dec val of
        Err err -> cb <| Err <| JsonParseError err
        Ok tp -> cb <| Ok tp
  )

{-| expectValue tells ZeroNet runtime that we expect raw Json.Encode.Value
-}
expectValue : ( Result Error Value -> msg ) -> Expect msg
expectValue = Val

processResponse : Expect msg -> Result CmdI.ZFrameError Value -> msg
processResponse ( Val cb ) res =
  res
    |> Result.mapError RequestError
    |> cb

{-| query returns command that tells ZeroNet runtime to execute database query

    query
      { query = "SELECT * FROM messages WHERE user_id = :user_id LIMIT 20;"
      , params = JE.object
        [ ( "user_id", model.userID )
        ]
      , expect = expectJson messageDecoder GotDatabaseQueryResult
      }
-}
query : QueryParams msg -> CmdI.Command msg
query prms =
  CmdI.ZFrame "dbQuery"
    ( JE.list identity <|
      [ JE.string prms.query
      , prms.params
      ] )
    ( CmdI.Response <| processResponse prms.expect )

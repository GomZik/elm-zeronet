module ZeroNet.Files exposing
  ( Error, put, get, Expect, expectText, expectJson, Content(..) )

import ZeroNet.Command.Internal as CmdI

import Json.Encode as JE exposing ( Value )
import Json.Decode as JD

import Base64

type Error
  = JsonError JD.Error
  | ZFrameError CmdI.ZFrameError
  | PutError String

type Content
  = Text String
  | Json Value

putResponseDecoder : JD.Decoder ( Result Error () )
putResponseDecoder =
  JD.oneOf
  [ JD.field "error" JD.string |> JD.andThen ( \str -> JD.succeed <| Err <| PutError str )
  , JD.succeed <| Ok ()
  ]

put : ( Result Error () -> msg ) -> String -> Content -> CmdI.Command msg
put cb name data =
  CmdI.ZFrame "fileWrite"
    ( JE.list identity
      [ JE.string name
      , JE.string <| Base64.encode <| case data of
        Text str -> str
        Json val -> JE.encode 1 val
      ]
    )
    ( CmdI.Response (\res ->
      case res of
        Err err -> cb <| Err <| ZFrameError err
        Ok val -> case JD.decodeValue putResponseDecoder val of
          Ok decres -> cb <| decres
          Err err -> cb <| Err <| JsonError err
    ) )

type Expect msg
  = TextResponse ( Result Error String -> msg )

expectText : ( Result Error String -> msg ) -> Expect msg
expectText cb =
  TextResponse cb

expectJson : ( JD.Decoder x ) -> ( Result Error x -> msg ) -> Expect msg
expectJson decoder cb =
  expectText (\res ->
    cb <| case res of
      Ok str -> JD.decodeString decoder str
        |> Result.mapError JsonError
      Err err -> Err err
  )


type alias GetParams msg =
  { path : String
  , required : Bool
  , expect : Expect msg
  , timeout : Maybe Int
  }

processGetResponse : Expect msg -> Result CmdI.ZFrameError Value -> msg
processGetResponse ( TextResponse cb ) result =
  cb <| case result of
    Ok val -> JD.decodeValue JD.string val
      |> Result.mapError JsonError
    Err err -> Err <| ZFrameError err

get : GetParams msg -> CmdI.Command msg
get prms =
  CmdI.ZFrame "fileGet"
    ( JE.list identity
      [ JE.string prms.path
      , JE.bool prms.required
      , JE.string "text"
      , case prms.timeout of
        Nothing -> JE.null
        Just tm -> JE.int tm
      ]
    )
    ( CmdI.Response <| processGetResponse prms.expect )

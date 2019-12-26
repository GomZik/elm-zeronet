module ZeroNet.Files exposing
  ( Error, put, get, Expect, expectText, expectJson, Content(..), onFileWrite
  , GetParams
  )

{-| ZeroNet.Files allows to work with ZeroNet files trough ZeroFrame API

# Types

@docs Error

# Reading files

@docs get, GetParams, Expect, expectText, expectJson

# Writing files

@docs put, Content

# Subscriptions

@docs onFileWrite
-}

import ZeroNet.Command.Internal as CmdI
import ZeroNet.Subscription.Internal as SubI

import Json.Encode as JE exposing ( Value )
import Json.Decode as JD

import Base64

{-| Error represents that something happend during command execution
-}
type Error
  = JsonError JD.Error
  | ZFrameError CmdI.ZFrameError
  | PutError String

{-| You can choose what to put.
-}
type Content
  = Text String
  | Json Value

putResponseDecoder : JD.Decoder ( Result Error () )
putResponseDecoder =
  JD.oneOf
  [ JD.field "error" JD.string |> JD.andThen ( \str -> JD.succeed <| Err <| PutError str )
  , JD.succeed <| Ok ()
  ]

{-| Puts file. You should provide an inner path and content to put

    put FileWriteResult ( "data/user/" ++ authCert ++ "/data.json" ) <|
      Json <| JE.object
        [ ( "next_message_id", model.nextMessageId )
        , ( "messages", JE.list messageDecoder )
        ]
-}
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

{-| Expect represents what we should expect on file read
-}
type Expect msg
  = TextResponse ( Result Error String -> msg )

{-| Returns Expect type that tells Elm-ZeroNet runtime, that we wait
for text response

-}
expectText : ( Result Error String -> msg ) -> Expect msg
expectText cb =
  TextResponse cb

{-| Returns Expect type that tells Elm-ZeroNet runtime that we wait for a decoded object

    type Msg = GetFileResult ( Result File.Error Message )

    type alias Message =
      {}

    messageDecoder : JD.Decoder Message
    messageDecoder = Debug.todo "messageDecoder"

    expectJson messageDecoder GetFileResult
-}
expectJson : ( JD.Decoder x ) -> ( Result Error x -> msg ) -> Expect msg
expectJson decoder cb =
  expectText (\res ->
    cb <| case res of
      Ok str -> JD.decodeString decoder str
        |> Result.mapError JsonError
      Err err -> Err err
  )


{-| GetParams is just alias for `get` params
-}
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

{-| get returns command that reads file and return file content

    get
      { path = "data/users/" ++ authCert ++ "/data.json"
      , requred = True
      , expect = expectJson messageDecoder GetFileResult
      , timeout = Nothing
      }
-}
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


{-| Fires when ZeroNet notifies site, that file was updated
-}
onFileWrite : msg -> SubI.Subscription msg
onFileWrite = SubI.OnFileWrite

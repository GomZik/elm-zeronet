module App.Data.Article exposing
  ( Article, decoder, title, id
  )

import Json.Decode as JD

type ParentArticle
  = None
  | NotPrefetched Int
  | Prefetched Article

type alias Internals =
  { id : Int
  , title : String
  , body : String
  , parentArticle : ParentArticle
  }

type Article = A Internals

title : Article -> String
title ( A data ) = data.title

id : Article -> Int
id ( A data ) = data.id

decoder : JD.Decoder Article
decoder =
  JD.map A <|
    JD.map4 Internals
      ( JD.field "id" JD.int )
      ( JD.field "title" JD.string )
      ( JD.field "body" JD.string )
      ( JD.field "parent_id"
        ( JD.maybe JD.int )
        |> JD.andThen (\maybeID ->
          case maybeID of
            Nothing -> JD.succeed None
            Just i -> JD.succeed <| NotPrefetched i
        )
      )

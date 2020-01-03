port module ZeroNet.Navigation exposing ( Key, Request(..), pushUrl, load )

{-| ZeroNet.Navigation allows to handle url changes

# Types

@docs Key, Request

# Commands

@docs pushUrl, load
-}

import ZeroNet.Navigation.Internal as I
import ZeroNet.Command.Internal as CmdI
import Json.Encode as JE

import Url exposing ( Url )

port goUrl : String -> Cmd msg

{-| A navigation Key is needed to create navigation commands that change the URL.
That includes pushUrl, <... TODO ...> replaceUrl, back, and forward.

You only get access to a Key when you create your program with ZeroNet.program,
guaranteeing that your program is equipped to detect these URL changes.
If Key values were available in other kinds of programs,
unsuspecting programmers would be sure to run into some annoying
bugs and learn a bunch of techniques the hard way!
-}
type alias Key = I.Key

{-| All links in an application create a UrlRequest.
So when you click `<a href="/home">Home</a>`, it does not just navigate!
It notifies onUrlRequest that the user wants to change the Url.

## Internal, Zite, External

Internal url means, that requested url is on the same zite

Zite url means, that requested url is on ZeroNet network, but different zite

External url means that it is clearnet url

This is useful because it gives you a chance to customize the behavior in each case.
Maybe on some Internal links you save the scroll position with Browser.Dom.getViewport
so you can restore it later. Maybe on External links you persist parts of the Model on your servers before leaving.
Whatever you need to do!
-}
type Request
  = Internal Url
  | Zite String
  | External String


{-| Change the URL, but do not trigger a page load.

This will add a new entry to the browser history.

Note: If the user has gone back a few pages,
there will be “future pages” that the user can go forward to.
Adding a new URL in that scenario will clear out any future pages.
It is like going back in time and making a different choice.
-}
pushUrl : Key -> String -> CmdI.Command msg
pushUrl _ url =
  CmdI.ZFrame
    "wrapperPushState"
    ( JE.list identity
      [ JE.null
      , JE.string "Title"
      , JE.string url
      ] )
    CmdI.NoResponse

{-| Leave the current page and load the given URL.
This always results in a page load, even if the provided URL is the same as the current one.

    gotoElmWebsite : Cmd msg
    gotoElmWebsite =
        load "https://elm-lang.org"
-}

load : String -> CmdI.Command msg
load = CmdI.Platform << goUrl

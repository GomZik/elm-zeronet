module ZeroNet.Command exposing ( Command, none, batch, cmd )

{-| ZeroNet.Command represents a command to ZeroNet and Elm runtime

# Types

@docs Command

# Functions

@docs none, batch, cmd
-}

import ZeroNet.Command.Internal as I


{-| Command is like Platform.Cmd, but adds some ZeroNet commands.

Command can be returned from other ZeroNet.* modules
-}
type alias Command msg = I.Command msg

{-| Tell the runtime that there are no commands.
-}
none : Command msg
none = I.None

{-| When you need the runtime system to perform a couple commands, you can batch them together.
Each is handed to the runtime at the same time, and since each can perform arbitrary
operations in the world, there are no ordering guarantees about the results.

Note: Cmd.none and Cmd.batch [ Cmd.none, Cmd.none ] and Cmd.batch [] all do the same thing.
-}
batch : List ( Command msg ) -> Command msg
batch = I.Batch

{-| When you need to pass default Elm Platform.Cmd - user this function
-}
cmd : Cmd msg -> Command msg
cmd = I.Platform

module Commands
  ( module CommandModules
  , commands
  , findCommand
  , printCommands
  ) where

import           Hacks
import           Types

import           Commands.Belongs as CommandModules
import           Commands.Clean   as CommandModules
import           Commands.Delete  as CommandModules
import           Commands.Digest  as CommandModules
import           Commands.Test    as CommandModules
import           Commands.Commit  as CommandModules
import           Commands.Find    as CommandModules
import           Commands.Get     as CommandModules
import           Commands.Live    as CommandModules
import           Commands.Update  as CommandModules
import           Commands.Updates as CommandModules
import           Commands.Upgrade as CommandModules
import           Commands.UwU     as CommandModules

import           Data.List

commands ∷ Bool -> [Command']
commands showPrivate =
    [ Command' getCmd,     Command' deleteCmd
    , Command' updateCmd,  Command' upgradeCmd
    , Command' updatesCmd, Command' liveCmd
    , Command' cleanCmd,   Command' commitCmd
    , Command' findCmd,    Command' belongsCmd
    , Command' digestCmd,  Command' testCmd
    ] ++ concat
    [ [ Command' uwuCmd
      ] | showPrivate ]

findCommand ∷ String -> Maybe Command'
findCommand x = lookup x [ (n,c') | c'@(Command' c) <- commands True
                                  , n <- command c ]

printCommands ∷ [Command'] -> String
printCommands = align ∘ map printCommand
  where printCommand (Command' cmd) =
          [intercalate ", " (command cmd), "  ", description cmd]

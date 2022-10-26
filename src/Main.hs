{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           Commands.Delete
import           Commands.Find
import           Commands.Get
import           Commands.Test
import           Commands.Update
import           Commands.Upgrade
import           Commands.Clean
import           Commands.Belongs

import           Types
import           Utils
import           Version

import           Data.List
import           System.Environment

commands ∷ Bool → [Command']
commands showPrivate =
    [ Command' getCmd
    , Command' findCmd
    , Command' deleteCmd
    , Command' updateCmd
    , Command' upgradeCmd
    , Command' cleanCmd
    , Command' belongsCmd
    ] ++ concat
    [
      [
        Command' testCmd
      ]
    | showPrivate ]

printHelp ∷ IO ()
printHelp = putStrLn $ showMyV ++ "\n\n" ++ printCommands (commands False)

findCommand ∷ String → Maybe Command'
findCommand x = lookup x [ (n,c') | c'@(Command' c) <- commands True, n <- command c ]

printCommands ∷ [Command'] → String
printCommands = align . map printCommand
  where printCommand (Command' cmd) =
          [intercalate ", " (command cmd), "  ", description cmd]

isHelp ∷ String → Bool
isHelp "-?"     =  True
isHelp "-h"     =  True
isHelp "--help" =  True
isHelp _        =  False

isHelps ∷ [String] → Bool
isHelps (x:_) =  isHelp x
isHelps _     =  False

handleCommand ∷ IORef PortageConfig → String → Command' → [String] → IO ()
handleCommand r cname (Command' c) args =
    if isHelps args
      then putStrLn (usageInfo (usage c cname) . options c $ False)
      else
        let (fs,n,es) = getOpt Permute (options c True) args
        in case es of
          [] -> handler c r (foldl (flip ($)) (state c) fs) n
          _  -> do putStrLn (unlines es)
                   putStrLn (usageInfo (usage c cname) . options c $ False)

goWithArguments ∷ [String] → IO ()
goWithArguments []              =  printHelp
goWithArguments ["--version"]   =  putStrLn showMyV
goWithArguments [a] | isHelp a  =  printHelp
goWithArguments (x:xs) =
  do r <- portageConfig >>= newIORef
     case findCommand x of
      Nothing -> handleCommand  r  "get"  (Command' getCmd)  (x:xs)
      Just c  -> handleCommand  r  x      c                     xs

main ∷ IO ()
main = getArgs >>= goWithArguments

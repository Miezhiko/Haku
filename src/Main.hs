{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Main where

import           Paths
import           Types
import           Utils
import           Version

import           Commands.Belongs
import           Commands.Clean
import           Commands.Delete
import           Commands.Find
import           Commands.Get
import           Commands.Test
import           Commands.Update
import           Commands.Upgrade
import           Commands.UwU

import           Data.List

import           System.Environment

import           Control.Concurrent.Async.Lifted.Safe

commands ∷ Bool → [Command']
commands showPrivate =
    [ Command' getCmd,    Command' deleteCmd
    , Command' updateCmd, Command' upgradeCmd
    , Command' cleanCmd
    , Command' findCmd,   Command' belongsCmd
    ] ++ concat
    [ [ Command' testCmd
      , Command' uwuCmd
      ] | showPrivate ]

findCommand ∷ String → Maybe Command'
findCommand x = lookup x [ (n,c') | c'@(Command' c) <- commands True, n <- command c ]

printCommands ∷ [Command'] → String
printCommands = align ∘ map printCommand
  where printCommand (Command' cmd) =
          [intercalate ", " (command cmd), "  ", description cmd]

printHelp ∷ IO ()
printHelp = putStrLn $ showMyV ++ "\n\n"
                               ++ printCommands (commands False)

isVersion ∷ String → Bool
isVersion "-v"        =  True
isVersion "--version" =  True
isVersion _           =  False

isHelp ∷ String → Bool
isHelp "-?"     =  True
isHelp "-h"     =  True
isHelp "--help" =  True
isHelp _        =  False

isHelps ∷ [String] → Bool
isHelps (x:_) =  isHelp x
isHelps _     =  False

handleCommand ∷ IORef PortageConfig → String → Command' → [String] → HakuEnv -> IO ()
handleCommand r cname (Command' c) args env =
    if isHelps args
      then putStrLn (usageInfo (usage c cname) ∘ options c $ False)
      else
        let (fs,n,es) = getOpt Permute (options c True) args
        in case es of
          [] -> do void $ runReaderT
                    (concurrently
                      (handler c r (foldl (flip ($)) (state c) fs) n)
                      (hakuLog ("running " ++ cname))
                    ) env
                   putStrLn "OK"
          _  -> do putStrLn (unlines es)
                   putStrLn (usageInfo (usage c cname) ∘ options c $ False)

hakuLog ∷ (MonadReader HakuEnv m, MonadIO m)
             ⇒ String
             → m ()
hakuLog msg = liftIO . flip logger msg =<< ask

goWithArguments ∷ [String] → IO ()
goWithArguments []                =  printHelp
goWithArguments [a] | isVersion a =  putStrLn showMyV
goWithArguments [a] | isHelp a    =  printHelp
goWithArguments (x:xs) = getHakuCachePath >>= \hakuCachePath ->
  withBinaryFile hakuCachePath ReadWriteMode $ \h -> do
    let env = HakuEnv
          { handle = h
          , logger = putStrLn
          }
    (pc,_) <- runReaderT
      (concurrently
        portageConfig
        (hakuLog "reading config")
      )
      env
    r <- newIORef pc
    case findCommand x of
      Nothing -> handleCommand  r  "get"  (Command' getCmd)  (x:xs) env
      Just c  -> handleCommand  r  x      c                     xs  env

main ∷ IO ()
main = getArgs >>= goWithArguments

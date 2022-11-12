{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Main where

import           Paths
import           Types
import           Utils
import           Version

import           Portage.Config                       (portageConfig)

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
import           Data.Time.Clock
import           Data.Time.Format                     (defaultTimeLocale, formatTime)

import           System.Console.ANSI
import           System.Environment

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
findCommand x = lookup x [ (n,c') | c'@(Command' c) ← commands True, n ← command c ]

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

hakuLogger ∷ String → IO ()
hakuLogger msg = do
  t ← formatTime defaultTimeLocale "%F %T" <$> getCurrentTime
  setSGR [ SetColor Foreground Vivid Magenta ]
  putStr t
  setSGR [ SetColor Foreground Dull Cyan
         , SetConsoleIntensity BoldIntensity ]
  putStrLn $ " " ++ msg
  setSGR [ Reset ]

hakuLog ∷ HakuMonad m ⇒ String → m ()
hakuLog msg = liftIO ∘ flip logger msg =<< ask

{-
  lifted-async possibility:
  (concurrently
    (handler c (foldl (flip ($)) (state c) fs) n)
    ...
  ) env
-}
handleCommand ∷ String → Command' → [String] → HakuEnv → IO ()
handleCommand cname (Command' c) args env =
  if isHelps args
    then putStrLn (usageInfo (usage c cname) ∘ options c $ False)
    else
      let (fs,xs,es) = getOpt Permute (options c True) args
      in case es of
        [] → do void $ runReaderT (
                  hakuLog ( "[CMD] executing " ++ cname
                         ++ case xs of
                              [] -> []
                              ss -> " with " ++ intercalate ", " ss
                         ++ "\n" )
                  ) env
                void $ runReaderT (
                  handler c (
                      foldl (flip ($)) (state c) fs
                    ) xs
                  ) env
        _  → putStrLn (unlines es)
          ≫ putStrLn (usageInfo (usage c cname) ∘ options c $ False)

goWithArguments ∷ [String] → IO ()
goWithArguments []                =  printHelp
goWithArguments [a] | isVersion a =  putStrLn showMyV
goWithArguments [a] | isHelp a    =  printHelp
goWithArguments (x:xs) = getHakuCachePath >>= \hakuCachePath →
  withBinaryFile hakuCachePath ReadWriteMode $ \h → do
    gentooConfig ← portageConfig h >>= newIORef
    let env = HakuEnv
          { handle = h
          , logger = hakuLogger
          , config = gentooConfig
          }
    case findCommand x of
      Nothing → handleCommand  "get"  (Command' getCmd)  (x:xs) env
      Just c  → handleCommand  x      c                     xs  env

main ∷ IO ()
main = getArgs >>= goWithArguments

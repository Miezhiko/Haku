module Main
  ( main
  ) where

import           Commands
import           Logger
import           Paths
import           Types
import           Version

import           Portage.Config

import           Data.List

import           Control.Monad       (when)

import           System.Console.ANSI
import           System.Directory    (doesFileExist)
import           System.Environment

printHelp ∷ IO ()
printHelp = do
  setSGR [ SetColor Foreground Dull Red
         , SetConsoleIntensity BoldIntensity
         , SetItalicized True ]
  putStrLn $ showMyV ++ "\n"
  setSGR [ Reset ]
  putStrLn $ printCommands (commands False)

hakuHandle ∷ Command τ (ReaderT HakuEnv IO)
          -> [τ -> τ]
          -> [String]
          -> HakuEnv -> IO ()
hakuHandle cmd ss xs = runReaderT (handleM cmd ss xs)
  where handleM ∷ HakuMonad m ⇒ Command τ m -> [τ -> τ] -> [String] -> m ()
        handleM c = handler c ∘ foldl (flip id) (state c)

handleCommand ∷ String -> Command' -> [String] -> IO ()
handleCommand cname (Command' c) args =
  if isHelps args
    then putStrLn (usageInfo (usage c cname) ∘ options c $ False)
    else let (fs,xs,es) = getOpt Permute (options c True) args
         in case es of
          [] -> handleWIthEnv xs fs
          _  -> putStrLn (unlines es)
            ≫ putStrLn (usageInfo (usage c cname) ∘ options c $ False)
 where
  handleWIthEnv xs fs =
    getHakuCachePath >>= \hakuCachePath -> do
    cacheExists <- doesFileExist hakuCachePath
    withBinaryFile hakuCachePath ReadWriteMode $ \h -> do
      gentooConfig <-
        if cacheExists then portageConfig (deps c) hakuCachePath h >>= newIORef
                        else loadPortageConfig (deps c) >>= newIORef
      let env = HakuEnv
            { handle = h
            , logger = hakuLogger
            , config = gentooConfig
            }
      hakuLog ( "[CMD] executing <Magenta>" ++ cname
                ++ case xs of [] -> []
                              ss -> "<Default> with <Magenta>" ++ intercalate ", " ss
                        ) env
      hakuHandle c fs xs env
      readIORef gentooConfig >>= \pc ->
        when (pcUpdateCache pc) $
          storeConfig h pc

goWithArguments ∷ [String] -> IO ()
goWithArguments []                =  printHelp
goWithArguments [a] | isVersion a =  putStrLn showMyV
goWithArguments [a] | isHelp a    =  printHelp
goWithArguments (x:xs) =
  case findCommand x of
    Nothing -> handleCommand  "get"  (Command' getCmd)  (x:xs)
    Just c  -> handleCommand  x      c                     xs

main ∷ IO ()
main = getArgs >>= goWithArguments

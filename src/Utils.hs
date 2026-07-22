module Utils
  ( module ExportedUtils
  , hasSudo
  , isRoot
  , isRootOrPretend
  , raw
  , rawAndIgnore
  , readIfSucc
  , readQuiet
  , runIfExists
  ) where

import           Constants           (cosntSudoPath)

import           Hacks               as ExportedUtils
import           Portage.Utils       as ExportedUtils
import           Prelude.Unicode

import           System.Directory    (doesFileExist)
import           System.Exit
import           System.Posix.User   (getRealUserID)
import           System.Process

import           Control.Exception
import           Control.Monad       as ExportedUtils

import           System.Console.ANSI

checkExitCode ∷ ExitCode -> IO ()
checkExitCode ExitSuccess = pure ()
checkExitCode (ExitFailure γ) =
    error $ "failed with exit code: " ++ show γ

raw ∷ String -> [String] -> IO ()
raw λ α = rawSystem λ α >>= checkExitCode

rawAndIgnore ∷ String -> [String] -> IO ()
rawAndIgnore = void .: rawSystem

runIfExists ∷ FilePath -> String -> [String] -> IO ()
runIfExists ξ λ α =
  doesFileExist ξ >>= \fe ->
    when fe $ void (rawSystem λ α)

readCheck     -- return whether command was success or not
   ∷ String   -- command
  -> [String] -- arguments
  -> IO (Either SomeException String)
readCheck γ args = try $ readProcess γ args []

readIfSucc    -- useless wrapper on readCheck to return Maybe
   ∷ String   -- command
  -> [String] -- arguments
  -> IO (Maybe String)
readIfSucc γ args =
  readCheck γ args
  >>= \case Left _    -> pure Nothing
            Right val ->
              case val of
                [] -> pure $ Nothing
                xs -> do putStr $ γ ++ " : " ++ xs
                         pure $ Just xs

readQuiet     -- like readIfSucc, but without the stdout echo,
              -- for querying tools (e.g. portageq) as plain config lookups
   ∷ String   -- command
  -> [String] -- arguments
  -> IO (Maybe String)
readQuiet γ args =
  readCheck γ args
  >>= \case Left _    -> pure Nothing
            Right val -> case rstrip val of
                          [] -> pure Nothing
                          xs -> pure $ Just xs

isRoot ∷ IO () -> IO () -> IO ()
isRoot f1 f2 = getRealUserID >>=
  (\r -> if r then f1
              else hasSudo f2) . (== 0)

-- like 'isRoot', but a pretend run needs neither root nor sudo
-- since nothing is actually mutated
isRootOrPretend ∷ Bool -> IO () -> IO () -> IO ()
isRootOrPretend pretend f1 f2 = getRealUserID >>=
  (\r -> if r ∨ pretend then f1
                        else hasSudo f2) . (== 0)

hasSudo ∷ IO () -> IO ()
hasSudo something = doesFileExist cosntSudoPath >>=
  \case True  -> messageRunningWithSudo
              >> something
        False -> messageShouldRunAsRoot

messageRunningWithSudo ∷ IO ()
messageRunningWithSudo = do
  setSGR [ SetColor Foreground Vivid Yellow
         , SetConsoleIntensity BoldIntensity ]
  putStr "running with sudo (not recommended)"
  setSGR [ Reset ]
  putStrLn [] -- forcing reset!

messageShouldRunAsRoot ∷ IO ()
messageShouldRunAsRoot = do
  setSGR [ SetColor Foreground Vivid Red
         , SetConsoleIntensity BoldIntensity ]
  putStr "should run as root or have sudo installed"
  setSGR [ Reset ]
  putStrLn [] -- forcing reset!

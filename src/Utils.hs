{-# LANGUAGE
    LambdaCase
  , UnicodeSyntax
  #-}

module Utils
  ( module ExportedUtils
  , hasSudo
  , isRoot
  , raw
  , rawAndIgnore
  , runIfExists
  ) where

import           Constants           (cosntSudoPath)

import           Hacks               as ExportedUtils
import           Portage.Utils       as ExportedUtils

import           System.Directory    (doesFileExist)
import           System.Exit
import           System.Posix.User   (getRealUserID)
import           System.Process

import           Control.Monad

import           System.Console.ANSI

checkExitCode ∷ ExitCode → IO ()
checkExitCode ExitSuccess = pure ()
checkExitCode (ExitFailure γ) =
    error $ "failed with exit code: " ++ show γ

raw ∷ String → [String] → IO ()
raw λ α = rawSystem λ α >>= checkExitCode

rawAndIgnore ∷ String → [String] → IO ()
rawAndIgnore = void .: rawSystem

runIfExists ∷ FilePath → String → [String] → IO ()
runIfExists ξ λ α =
  doesFileExist ξ >>= \fe →
    when fe $ void (rawSystem λ α)

isRoot ∷ IO () → IO () → IO ()
isRoot f1 f2 = getRealUserID >>=
  (\r -> if r then f1
              else hasSudo f2) . (== 0)

hasSudo ∷ IO () → IO ()
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

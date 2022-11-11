{-# LANGUAGE
    UnicodeSyntax
  #-}

module Utils
  ( module ExportedUtils
  , raw
  , rawAndIgnore
  , runIfExists
  ) where

import           Hacks            as ExportedUtils
import           Portage.Utils    as ExportedUtils

import           System.Directory (doesFileExist)
import           System.Exit
import           System.Process

import           Control.Monad

checkExitCode ∷ ExitCode → IO ()
checkExitCode ExitSuccess = return ()
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

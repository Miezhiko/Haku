{-# LANGUAGE UnicodeSyntax #-}
module Utils
  ( module ExportedUtils
  , align
  , raw
  , rawAndIgnore
  , runIfExists
  ) where

import           Portage.Utils    as ExportedUtils

import           Data.List

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
rawAndIgnore = (void .) . rawSystem

runIfExists ∷ FilePath → String → [String] → IO ()
runIfExists ξ λ α =
  doesFileExist ξ >>= \fe ->
    when fe $ void (rawSystem λ α)

align ∷ [[String]] → String
align ts =  let maxlengths = map (maximum . map length) (transpose ts)
            in  unlines . map (concat . zipWith formatIn maxlengths) $ ts
  where  formatIn ∷ Int → String → String
         formatIn n s = s ++ replicate (n - length s) ' '

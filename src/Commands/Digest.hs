{-# LANGUAGE
    LambdaCase
  , UnicodeSyntax
  #-}

module Commands.Digest where

import           Constants         (cosntSudoPath)
import           Types
import           Utils

import           Data.List         (isSuffixOf)

import           System.Directory  (doesFileExist, getCurrentDirectory, getDirectoryContents)
import           System.Posix.User (getRealUserID)

{- HLINT ignore "Redundant <$>" -}
digestEbuild ∷ String → IO ()
digestEbuild x = (== 0) <$> getRealUserID >>= \r →
  if r then rawAndIgnore "ebuild" [ x, "digest" ]
      else doesFileExist cosntSudoPath >>= \sudoExists →
        if sudoExists then rawAndIgnore "sudo" [ "ebuild", x, "digest" ]
                      else putStrLn "should run as root or have sudo installed"

digest ∷ IO ()
digest = filter (isSuffixOf ".ebuild") <$> (getDirectoryContents =<< getCurrentDirectory)
         >>= \case []    -> putStrLn "no ebuilds found"
                   (x:_) -> digestEbuild x

digestCmd ∷ Command String m
digestCmd = Command { command = ["digest"]
                   , description = "update manifest hash for ebuilds"
                   , usage = ("haku " ++)
                   , state = 𝜀
                   , options = const 𝜀
                   , handler = \_ _ → liftIO digest }

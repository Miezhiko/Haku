module Commands.Digest
  ( digestCmd
  ) where

import           Types
import           Utils

import           Data.List        (isSuffixOf)

import           System.Directory (getCurrentDirectory, getDirectoryContents)

digestEbuild ∷ String -> IO ()
digestEbuild x = isRoot
  ( rawAndIgnore "ebuild" [ x, "digest" ] )
  ( rawAndIgnore "sudo" [ "ebuild", x, "digest" ] )

{- HLINT ignore "Redundant <$>" -}
digest ∷ IO ()
digest = filter (isSuffixOf ".ebuild") <$> (getDirectoryContents =<< getCurrentDirectory)
         >>= \case []    -> putStrLn "no ebuilds found"
                   (x:_) -> digestEbuild x

digestCmd ∷ Command String m
digestCmd = Command { command      = ["digest"]
                    , deps         = []
                    , description  = "update manifest hash for ebuilds"
                    , usage        = ("haku " ++)
                    , state        = 𝜀
                    , options      = const 𝜀
                    , handler      = \_ _ -> liftIO digest }

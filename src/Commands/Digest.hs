module Commands.Digest
  ( digest
  , digestCmd
  ) where

import           Types
import           Utils

import           Data.List         (isSuffixOf)

import           System.Directory  (getCurrentDirectory, getDirectoryContents)
import           System.Exit       (ExitCode (..))
import           System.Posix.User (getRealUserID)
import           System.Process    (rawSystem)

runCommand ∷ String → [String] → IO Bool
runCommand cmd args = do
  result ← rawSystem cmd args
  pure $ case result of
    ExitSuccess   -> True
    ExitFailure _ -> False

digestEbuild ∷ String -> IO Bool
digestEbuild x = isRootBool
  ( runCommand "ebuild" [ x, "digest" ] )
  ( runCommand "sudo" [ "ebuild", x, "digest" ] )
 where isRootBool ∷ IO Bool → IO Bool → IO Bool
       isRootBool f1 f2 = do
          uid ← getRealUserID
          if uid == 0
            then f1
            else f2 

{- HLINT ignore "Redundant <$>" -}
digest ∷ IO Bool
digest = filter (isSuffixOf ".ebuild") <$> (getDirectoryContents =<< getCurrentDirectory)
         >>= \case []    -> putStrLn "no ebuilds found" >> pure False
                   (x:_) -> digestEbuild x

digestCmd ∷ Command String m
digestCmd = Command { command      = ["digest"]
                    , deps         = []
                    , description  = "update manifest hash for ebuilds"
                    , usage        = ("haku " ++)
                    , state        = 𝜀
                    , options      = const 𝜀
                    , handler      = \_ _ -> liftIO $ void digest }

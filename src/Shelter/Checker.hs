module Shelter.Checker
  ( ShelterConfig
  , getShelterConfig
  , updateAll
  ) where

import           Utils

import           Shelter.Types

import           Data.List.Split

import           System.Directory
import           System.FilePath
import           System.Process

getRemoteHash ∷ IO (Maybe String)
getRemoteHash = do
  currentBranch <- readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] []
  rlm <- readIfSucc "git" ["ls-remote", "origin", trim currentBranch]
  case rlm of
    Nothing  -> pure Nothing
    Just rlc -> pure $ Just (head (splitOn "\t" rlc))

checkForHash ∷ String -> Maybe String -> IO Bool
checkForHash rlc Nothing = do
  currentHash <- readProcess "git" ["log", "-n", "1"
                                  , "--pretty=format:%H"
                                  ] []
  pure $ rlc ≡ trim currentHash
checkForHash rlc (Just shelterHash) =
  pure $ rlc ≡ shelterHash

updateNode ∷ ShelterNode -> IO ShelterNode
updateNode node = do
  putStrLn $ target node
  getRemoteHash >>=
   \case Nothing  -> pure node
         Just r   -> checkForHash r (hash node) >>=
          \case False -> pure node
                True  -> do
                  rawAndIgnore "git" ("pull" : words (upstream node))
                  pure $ node { hash = Just r }

updateAll ∷ IO ()
updateAll = getShelterConfig >>= \case
  Nothing -> pure ()
  Just shelter ->
    traverse (\node ->
      let path = target node
      in doesDirectoryExist (path </> ".git") >>=
        \case True  -> setCurrentDirectory path
                    >> updateNode node
              False -> pure node) shelter
    >>= updateShelterConfig

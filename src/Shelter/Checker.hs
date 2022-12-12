{-# LANGUAGE
    LambdaCase
  , UnicodeSyntax
  #-}

module Shelter.Checker
  ( ShelterConfig
  , getShelterConfig
  , updateAll
  ) where

import           Utils

import           Shelter.Trim
import           Shelter.Types

import           Data.List.Split

import           Control.Exception

import           System.Directory
import           System.FilePath
import           System.Process

readCheck    -- return whether command was success or not
   ∷ String  -- command
  → [String] -- arguments
  → IO (Either SomeException String)
readCheck γ args = try $ readProcess γ args []

readIfSucc   -- useless wrapper on readCheck to return Maybe
   ∷ String  -- command
  → [String] -- arguments
  → IO (Maybe String)
readIfSucc γ args =
  readCheck γ args
  >>= \case Left _ → return Nothing
            Right val → do putStr $ γ ++ " : " ++ val
                           return (Just val)

getRemoteHash ∷ IO (Maybe String)
getRemoteHash = do
  currentBranch ← readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] []
  rlm ← readIfSucc "git" ["ls-remote", "origin", trim currentBranch]
  case rlm of
    Nothing  → return Nothing
    Just rlc → return $ Just (head (splitOn "\t" rlc))

checkForHash ∷ String → Maybe String → IO Bool
checkForHash rlc Nothing = do
  currentHash ← readProcess "git" ["log", "-n", "1"
                                   , "--pretty=format:%H"
                                   ] []
  return $ rlc == trim currentHash
checkForHash rlc (Just shelterHash) =
  return $ rlc == shelterHash

updateNode ∷ ShelterNode → IO ShelterNode
updateNode node = do
  getRemoteHash >>=
   \case Nothing  -> return node
         Just r   -> checkForHash r (hash node) >>=
          \case False -> return node
                True  -> do
                  rawAndIgnore "git" ["pull", upstream node]
                  return $ node { hash = Just r }

updateAll ∷ IO ()
updateAll = getShelterConfig >>= \case
  Nothing -> pure ()
  Just shelter ->
    mapM (\node ->
      let path = target node
      in doesDirectoryExist (path </> ".git") >>=
        \case True  -> withCurrentDirectory path (updateNode node)
              False -> pure node) shelter
    >>= updateShelterConfig

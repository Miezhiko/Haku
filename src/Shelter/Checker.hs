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
  >>= \case Left _ -> pure Nothing
            Right val -> do putStr $ γ ++ " : " ++ val
                            pure $ Just val

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
  pure $ rlc == trim currentHash
checkForHash rlc (Just shelterHash) =
  pure $ rlc == shelterHash

updateNode ∷ ShelterNode -> IO ShelterNode
updateNode node = do
  getRemoteHash >>=
   \case Nothing  -> pure node
         Just r   -> checkForHash r (hash node) >>=
          \case False -> pure node
                True  -> do
                  rawAndIgnore "git" ["pull", upstream node]
                  pure $ node { hash = Just r }

updateAll ∷ IO ()
updateAll = getShelterConfig >>= \case
  Nothing -> pure ()
  Just shelter ->
    traverse (\node ->
      let path = target node
      in doesDirectoryExist (path </> ".git") >>=
        \case True  -> withCurrentDirectory path (updateNode node)
              False -> pure node) shelter
    >>= updateShelterConfig

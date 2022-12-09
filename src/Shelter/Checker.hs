{-# LANGUAGE
    LambdaCase
  , UnicodeSyntax
  #-}

module Shelter.Checker
  ( ShelterConfig
  , getShelterConfig
  , isAllRepositoriesUpdated
  , isRepositoryUpdated
  , updateAll
  ) where

import           Utils

import           Shelter.Trim
import           Shelter.Types

import           Data.Foldable     (for_)
import           Data.List
import           Data.List.Split

import           Control.Exception
import           Control.Monad

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

checkForExistingHash ∷ String → IO Bool
checkForExistingHash shelterHash = do
  currentBranch ← readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] []
  -- note that this can be slow actually, maybe there is better way to do it
  rlm ← readIfSucc "git" ["ls-remote", "origin", trim currentBranch]
  case rlm of
    Nothing  → return False
    Just rlc → let remoteHash = head (splitOn "\t" rlc)
                in return $ remoteHash == shelterHash

checkForHash ∷ Maybe String → IO Bool
checkForHash Nothing     = do
  currentHash ← readProcess "git" ["log", "-n", "1"
                                   , "--pretty=format:%H"
                                   ] []
  checkForExistingHash (trim currentHash)
checkForHash (Just shelterHash) = checkForExistingHash shelterHash

checkForRepo ∷ Maybe ShelterConfig → FilePath → IO Bool
checkForRepo Nothing path =
  withCurrentDirectory path $ checkForHash Nothing
checkForRepo (Just shelter) path =
  checkForNode (find ((path ==) . target) shelter) path
 where checkForNode ∷ Maybe ShelterNode → FilePath → IO Bool
       checkForNode Nothing _     = return False
       checkForNode (Just n) rpath =
        withCurrentDirectory rpath
          $ checkForHash (hash n)
  
isRepositoryUpdated ∷ Maybe ShelterConfig → FilePath → IO Bool
isRepositoryUpdated shelter path =
  doesDirectoryExist (path </> ".git") >>= \dirExists →
    if dirExists -- assume that git exists in system
      then checkForRepo shelter path
      else return False

isShelterRepositoryUpdated ∷ ShelterNode → IO Bool
isShelterRepositoryUpdated node =
  let path = target node
  in doesDirectoryExist (path </> ".git") >>= \dirExists →
    if dirExists
      then withCurrentDirectory path $ checkForHash (hash node)
      else return False

isAllRepositoriesUpdated ∷ ShelterConfig → IO Bool
isAllRepositoriesUpdated = allM isShelterRepositoryUpdated

updateAll ∷ IO ()
updateAll = getShelterConfig >>= \case
  Nothing -> return ()
  Just shelter -> for_ shelter $ \node ->
    let path = target node
    in doesDirectoryExist (path </> ".git") >>= \dirExists → when dirExists $
      withCurrentDirectory path $ checkForHash (hash node) >>= \upadated ->
        unless upadated $ rawAndIgnore "git" ["pull", upstream node]

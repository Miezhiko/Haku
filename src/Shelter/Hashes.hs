{-# LANGUAGE
    LambdaCase
  , UnicodeSyntax
  #-}

module Shelter.Hashes
  ( ShelterConfig
  , getShelterConfig
  , getShelterHashes
  , isPortageConfigIsInSync
  ) where

import           Portage.Types.Config

import           Shelter.Types

import qualified Data.Map             as M

populateHashesMap ∷ ShelterConfig → M.Map FilePath (Maybe String)
populateHashesMap shelter = M.fromList listMap
 where listMap = map (\s -> (target s, hash s)) shelter

getShelterHashes ∷ IO (M.Map FilePath (Maybe String))
getShelterHashes = getShelterConfig >>=
  \case Nothing -> return M.empty
        Just sh -> return $ populateHashesMap sh

isShelterRepositoryInSync ∷ ShelterHashes → ShelterNode  → Bool
isShelterRepositoryInSync hashes n =
  case M.lookup (target n) hashes of
    Just h  -> h == hash n
    Nothing -> False

isPortageConfigIsInSync ∷ PortageConfig → ShelterConfig → Bool
isPortageConfigIsInSync = all . isShelterRepositoryInSync . pcShelterHashes

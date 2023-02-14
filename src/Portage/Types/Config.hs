{-# LANGUAGE
    DeriveGeneric
  , UnicodeSyntax
  #-}
module Portage.Types.Config where

import           Portage.Types.Masking
import           Portage.Types.Package

import           GHC.Generics          (Generic)

import           Data.Binary
import qualified Data.Map              as M

type EnvMap = M.Map String String

type Atom     = String -- Atom here is category/PN
type Tree     = M.Map Atom Package

type Overlays = M.Map String -- Overlay Name
                      ( FilePath
                      , [(String, [String])]
                      , [Masking]
                      )

type ShelterHashes = M.Map FilePath (Maybe String)

data PortageConfig
  = PortageConfig
      { pcMakeConf      :: EnvMap
      , pcCategories    :: [(String, [String])]
      , pcEclasses      :: [String]
      , pcTree          :: Tree
      , pcOverlays      :: Overlays
      , pcShelterHashes :: ShelterHashes
      , pcUpdateCache   :: Bool
      }
  deriving (Generic)

instance Binary PortageConfig

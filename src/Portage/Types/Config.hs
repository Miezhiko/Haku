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

type ConfData = M.Map String String
type Atom     = String -- Atom here is category/PN

data OverlayData
  = OverlayData
      { ovFilePath   :: FilePath
      , ovCategories :: [(String, [String])]
      , ovMasking    :: [Masking]
      }
  deriving (Generic)

data MaskingData
  = MaskingData
      { pMask     :: [Masking]
      , pKeywords :: [Masking]
        -- unused for now
      }
  deriving (Generic)

type Tree           = M.Map Atom Package
type ShelterHashes  = M.Map FilePath (Maybe String)
type Overlays       = M.Map String OverlayData

type OverlayMeta = ( String, OverlayData )

data PortageConfig
  = PortageConfig
      { pcMakeConf      :: ConfData
      , pcCategories    :: [(String, [String])]
      , pcEclasses      :: [String]
      , pcTree          :: Tree
      , pcOverlays      :: Overlays
      , pcMasking       :: MaskingData
      , pcShelterHashes :: ShelterHashes
      , pcUpdateCache   :: Bool
      }
  deriving (Generic)

instance Binary OverlayData
instance Binary MaskingData
instance Binary PortageConfig

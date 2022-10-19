{-# LANGUAGE UnicodeSyntax #-}
module Portage.Types.Config where

import           Portage.Package

import qualified Data.Map        as M

type EnvMap = M.Map String String

-- Atom is category/PN
type Atom     = String
type Tree     = M.Map Atom Package
type Overlays = M.Map String (FilePath, [(String, [String])])
type OverlayMeta =  (String, (FilePath, [(String, [String])]))

data PortageConfig
  = PortageConfig
      { pcMakeConf   :: EnvMap
      , pcCategories :: [(String, [String])]
      , pcTree       :: Tree
      , pcOverlays   :: Overlays
      }

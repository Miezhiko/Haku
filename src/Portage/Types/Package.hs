{-# LANGUAGE UnicodeSyntax #-}
module Portage.Types.Package
  ( Package (..)
  ) where

import           Portage.Types.Version

import           Data.Set

data Package
  = Package
      { pCategory  :: String
      , pVersions  :: Set PackageVersion
      , pName      :: String
      }

instance Show Package where
  show (Package c _ n) = c ++ "/" ++ n

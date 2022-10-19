{-# LANGUAGE UnicodeSyntax #-}
module Portage.Package
  ( Package (..)
  ) where

import           Portage.Version

import           Data.Set

data Package
  = Package
      { pCategory  :: String
      , pVersions  :: Set PackageVersion
      , pName      :: String
      }

instance Show Package where
  show (Package c _ n) = c ++ "/" ++ n

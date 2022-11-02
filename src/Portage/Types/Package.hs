{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}
module Portage.Types.Package
  ( Package (..)
  ) where

import           Portage.Types.Version

import           GHC.Generics          (Generic)

import           Data.Binary
import           Data.Set

data Package
  = Package
      { pCategory :: String
      , pVersions :: Set PackageVersion
      , pName     :: String
      }
  deriving (Generic)

instance Binary Package

instance Show Package where
  show (Package c _ n) = c ++ "/" ++ n

{-# LANGUAGE UnicodeSyntax #-}
module Portage.Package where

import           Portage.Version

data Package
  = Package
      { pCategory :: String
      , pVersions :: [PackageVersion]
      , pName     :: String
      }

instance Show Package where
  show (Package c _ n) = c ++ "/" ++ n

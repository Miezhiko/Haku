{-# LANGUAGE
    DeriveGeneric
  , UnicodeSyntax
  #-}
module Portage.Types.Package
  ( Package (..)
  , prettyPrintVersions
  , prettyShowVersions
  ) where

import           Portage.Types.Version

import           GHC.Generics          (Generic)

import           Data.Binary
import           Data.List             (intercalate)
import qualified Data.Set              as S

data Package
  = Package
      { pCategory :: String
      , pVersions :: S.Set PackageVersion
      , pName     :: String
      }
  deriving (Generic)

instance Binary Package

instance Show Package where
  show (Package c _ n) = c ++ "/" ++ n

prettyShowVersions ∷ S.Set PackageVersion → String
prettyShowVersions = intercalate ", " . map show . S.toList

prettyPrintVersions ∷ S.Set PackageVersion → IO ()
prettyPrintVersions = putStrLn . prettyShowVersions

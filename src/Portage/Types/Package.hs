module Portage.Types.Package
  ( Package (..)
  , prettyPrintVersions
  , prettyShowVersions
  , prettyShowVersionsList
  ) where

import           Prelude.Unicode

import           Portage.Types.Version

import           GHC.Generics          (Generic)

import           System.FilePath       ((</>))

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
  show (Package c _ n) = c </> n

prettyShowVersionsList ∷ [PackageVersion] -> String
prettyShowVersionsList = intercalate ", " ∘ map show

prettyShowVersions ∷ S.Set PackageVersion -> String
prettyShowVersions = prettyShowVersionsList ∘ S.toList

prettyPrintVersions ∷ S.Set PackageVersion -> IO ()
prettyPrintVersions = putStrLn ∘ prettyShowVersions

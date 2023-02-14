{-# LANGUAGE
    DeriveGeneric
  , UnicodeSyntax
  #-}
module Portage.Types.Masking where

import           Portage.Types.Atom

import           GHC.Generics       (Generic)

import           Data.Binary

data Masking
  = Masking
      { mreason  :: [String]
      , mfile    :: FilePath
      , mdepatom :: DepAtom
      , mnegate  :: Bool
      }
  deriving (Eq, Generic, Ord, Show)

instance Binary Masking

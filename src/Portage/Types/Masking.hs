module Portage.Types.Masking where

import           Portage.Types.Atom

import           GHC.Generics       (Generic)

import           Data.Binary

data Masking
  = Masking
      { mNegate  :: Bool
      , mSystem  :: Bool
      , mDepAtom :: DepAtom
      }
  deriving (Eq, Generic, Ord, Show)

instance Binary Masking

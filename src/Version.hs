{-# LANGUAGE UnicodeSyntax #-}

module Version
  ( showMyV
  ) where

import           Data.Version (showVersion)
import qualified Paths_Haku   as My

showMyV     ∷ String
showMyV     = "Haku v" ++ showVersion My.version

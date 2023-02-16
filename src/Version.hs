{-# LANGUAGE
    UnicodeSyntax
  #-}

module Version
  ( isHelp
  , isHelps
  , isVersion
  , showMyV
  ) where

import           Data.Version (showVersion)
import qualified Paths_Haku   as My

isVersion ∷ String -> Bool
isVersion "-v"        =  True
isVersion "--version" =  True
isVersion _           =  False

isHelp ∷ String -> Bool
isHelp "-?"     =  True
isHelp "-h"     =  True
isHelp "--help" =  True
isHelp _        =  False

isHelps ∷ [String] -> Bool
isHelps (x:_) =  isHelp x
isHelps _     =  False

showMyV     ∷ String
showMyV     = "Haku v" ++ showVersion My.version

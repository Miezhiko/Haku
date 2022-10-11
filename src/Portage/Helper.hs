{-# LANGUAGE UnicodeSyntax #-}
module Portage.Helper where

import           Data.Bifunctor
import qualified Data.Map       as M

readStringMap ∷ [String] → M.Map String String
readStringMap = M.fromList . map (second tail . break (=='='))

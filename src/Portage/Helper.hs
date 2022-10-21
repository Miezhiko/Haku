{-# LANGUAGE UnicodeSyntax #-}
module Portage.Helper where

import           Data.Bifunctor
import           Data.Char      (isSpace)
import qualified Data.Map       as M

rstrip ∷ String → String
rstrip = reverse . dropWhile isSpace . reverse

readStringMap ∷ [String] → M.Map String String
readStringMap = M.fromList . map (second tail . break (=='='))

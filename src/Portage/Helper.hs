{-# LANGUAGE UnicodeSyntax #-}
module Portage.Helper where

import           Data.Bifunctor
import           Data.Char       (isSpace)
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M

rstrip ∷ String → String
rstrip = reverse . dropWhile isSpace . reverse

readStringMap ∷ [String] → M.Map String String
readStringMap = M.fromList . map (second tail . break (=='='))

splitOnAnyOf ∷ Eq a ⇒ [[a]] → [a] → [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

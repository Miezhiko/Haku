{-# LANGUAGE UnicodeSyntax #-}
module Portage.Helper where

import           Data.Bifunctor
import           Data.Char        (isSpace)
import           Data.List
import           Data.List.Split
import qualified Data.Map         as M

import           System.Directory
import           System.FilePath  ((</>))

rstrip ∷ String → String
rstrip = reverse . dropWhile isSpace . reverse

readStringMap ∷ [String] → M.Map String String
readStringMap = M.fromList . map (second tail . break (=='='))

splitOnAnyOf ∷ Eq a ⇒ [[a]] → [a] → [[a]]
splitOnAnyOf ds xs = foldl' ((. splitOn) . (>>=)) [xs] ds

getHakuCachePath ∷ IO FilePath
getHakuCachePath = (</> "haku.cache") <$> getHomeDirectory

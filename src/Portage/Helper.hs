{-# LANGUAGE UnicodeSyntax #-}
module Portage.Helper where

import           Data.Bifunctor
import           Data.Char      (isSpace)
import qualified Data.Map       as M
import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO

rstrip ∷ String → String
rstrip = reverse . dropWhile isSpace . reverse

readStringMap ∷ [String] → M.Map String String
readStringMap = M.fromList . map (second tail . break (=='='))

readFileStrict ∷ FilePath → IO String
readFileStrict = fmap T.unpack . TIO.readFile

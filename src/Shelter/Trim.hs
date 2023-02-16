{-# LANGUAGE
    Safe
  , UnicodeSyntax
  #-}

module Shelter.Trim
  ( trim
  ) where

import           Data.Char (isSpace)

trim ∷ String -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail ∷ String -> String -> String
dropSpaceTail _ "" = ""
dropSpaceTail maybeStuff (χ:xs)
  | isSpace χ       = dropSpaceTail (χ:maybeStuff) xs
  | null maybeStuff = χ : dropSpaceTail "" xs
  | otherwise       = reverse maybeStuff ++ χ : dropSpaceTail "" xs

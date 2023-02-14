{-# LANGUAGE
    UnicodeSyntax
  #-}
module Portage.Mask
  ( module Portage.Types.Masking
  , normalizeMasking
  , parseMask
  ) where

import           Data.Char             (isSpace)
import qualified Data.Set              as S

import           Portage.Atom
import           Portage.Types.Masking

parseMask ∷ FilePath → String → [Masking]
parseMask f = parseMaskByLine f [] [] . lines

parseMaskByLine ∷ FilePath → [String] → [String] → [String] → [Masking]
parseMaskByLine f acc _acc (l@('#':_) : ls)  =  let  nacc = l : acc
                                                in   parseMaskByLine f nacc (reverse nacc) ls
parseMaskByLine f _ac facc (l@('-':_) : ls)  =  Masking facc f (getDepAtom l) True  : parseMaskByLine f [] facc ls
parseMaskByLine f _ac facc (l : ls)
  | all isSpace l                            =  parseMaskByLine f [] [] ls
  | otherwise                                =  Masking facc f (getDepAtom l) False : parseMaskByLine f [] facc ls
parseMaskByLine _ _ac _acc []                =  []

normalizeMasking ∷ [Masking] → [Masking]
normalizeMasking xs  =  S.elems $
                        foldl  (\s m -> if mnegate m  then  S.delete m s
                                                      else  S.insert m s
                               )
                               S.empty
                               xs

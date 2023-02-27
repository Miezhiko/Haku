{-# LANGUAGE
    UnicodeSyntax
  #-}
module Portage.Mask
  ( module Portage.Types.Masking
  , parseMask
  ) where

import           Portage.Atom
import           Portage.Types.Masking

import           Data.Functor.Identity

import           Text.Parsec.Prim
import           Text.ParserCombinators.Parsec as P

-- | Strip empty lines and comments from a string.
stripComments ∷ String -> String
stripComments = unlines . filter (not . null) . map (takeWhile (/= '#')) . lines

parseMask ∷ String -> [Masking]
parseMask = map getProfilePackage . lines . stripComments

-- | Get a dependency atom which can be modified by an initial @*@,
--   indicating a base system package, and by an additional initial @-@,
--   indicating removal of a package from the profile.
getProfilePackage ∷ String -> Masking
getProfilePackage p =  case parseProfilePackage p of
  Left   e ->  error $ "getProfilePackage: " ++ show e
  Right  x ->  x

parseProfilePackage ∷ [Char] -> Either ParseError Masking
parseProfilePackage = parse readProfilePackage "<-*depatom>"

readProfilePackage ∷ Text.Parsec.Prim.ParsecT
                      [Char] u Data.Functor.Identity.Identity Masking
readProfilePackage =  do  neg  <-  optchar '-'
                          sys  <-  optchar '*'
                          d    <-  readDepAtom (const [])
                          pure $ Masking neg sys d

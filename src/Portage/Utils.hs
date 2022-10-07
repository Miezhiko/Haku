{-# LANGUAGE UnicodeSyntax #-}
module Portage.Utils where

import           Portage.Config

import           Data.List
import qualified Data.Map       as M

findPackageByName ∷ PortageConfig → String → Maybe Package
findPackageByName pc x =
  let tree = pcTree pc
      cat = find (\c -> case M.lookup (c ++ "/" ++ x) tree of
                  Just _  -> True
                  Nothing -> False
                 ) $ pcCategories pc
  in case cat of
      Just c  -> findPackage pc (c ++ "/" ++ x)
      Nothing -> Nothing

findPackage ∷ PortageConfig → String → Maybe Package
findPackage pc input =
  if '/' `elem` input
    then M.lookup input (pcTree pc)
    else findPackageByName pc input

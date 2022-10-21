{-# LANGUAGE UnicodeSyntax #-}
module Portage.Ebuild where

import           Portage.Helper

import qualified Data.Map      as M
import           Data.Maybe

data Ebuild
  = Ebuild
      { eDepend      :: String
      , eRdepend     :: String
      , eSlot        :: String
      , eSrc_uri     :: String
      , eRestrict    :: [String]
      , eHomepage    :: String
      , eLicense     :: String
      , eDescription :: String
      , eKeywords    :: [String]
      , eInherited   :: [String]
      , eIuse        :: [String]
      , eCdepend     :: String
      , ePdepend     :: String
      , eProvide     :: String
      , eEapi        :: String
      }
  deriving (Eq, Show)

stringSeq ∷ String → b → b
stringSeq []      c =  c
stringSeq (_:xs)  c =  stringSeq xs c

strictReadFile ∷ FilePath → IO String
strictReadFile f  =   do  ff <- readFile f
                          ff `stringSeq` return ff

(!.) ∷ M.Map String String → String → String
m !. k = fromMaybe "" (M.lookup k m)

getEbuild ∷ FilePath → IO Ebuild
getEbuild f = do
  e <- fmap lines (strictReadFile f)
  let em = readStringMap e
  return (Ebuild  (em !. "DEPEND")
                  (em !. "RDEPEND")
                  (em !. "SLOT")
                  (em !. "SRC_URI")
                  [em !. "RESTRICT"]
                  (em !. "HOMEPAGE")
                  (em !. "LICENSE")
                  (em !. "DESCRIPTION")
                  [em !. "KEYWORDS"]
                  [] -- inherit
                  [em !. "IUSE"]
                  (em !. "CDEPEND")
                  (em !. "PDEPEND")
                  (em !. "PROVIDE")
                  (em !. "EAPI"))
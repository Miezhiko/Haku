{-# LANGUAGE
    UnicodeSyntax
  #-}
module Portage.Ebuild where

import           Hacks

import           Data.List
import qualified Data.Map  as M

data Ebuild
  = Ebuild
      { eDepend      :: [String]
      , eRdepend     :: [String]
      , eSlot        :: String
      , eSrc_uri     :: [String]
      , eRestrict    :: [String]
      , eHomepage    :: String
      , eLicense     :: String
      , eDescription :: String
      , eKeywords    :: [String]
      , eInherited   :: [String]
      , eIuse        :: [String]
      , eCdepend     :: [String]
      , ePdepend     :: [String]
      , eEapi        :: String
      }
  deriving (Eq, Show)

removeJunk ∷ String → String
removeJunk xs = [ x | x <- xs, x /= '\"' ]

stringSeq ∷ String → b → b
stringSeq []      c =  c
stringSeq (_:xs)  c =  stringSeq xs c

-- TODO: maybe use Strict.readFile instead
strictReadFile ∷ FilePath → IO String
strictReadFile f  =   do  ff <- readFile f
                          ff `stringSeq` return ff

(!.) ∷ M.Map String String → String → String
m !. k = maybe "" removeJunk (M.lookup k m)

getEbuild ∷ FilePath → IO Ebuild
getEbuild f = do
  e <- lines <$> strictReadFile f
  let em = readStringMap e
      inh = find (`isPrefixOf` "inherit ") e
      inherts = case inh of
                  Just s  -> drop 8 s
                  Nothing -> []
  return (Ebuild  (words (em !. "DEPEND"))
                  (words (em !. "RDEPEND"))
                  (em !. "SLOT")
                  (words (em !. "SRC_URI"))
                  (words (em !. "RESTRICT"))
                  (em !. "HOMEPAGE")
                  (em !. "LICENSE")
                  (em !. "DESCRIPTION")
                  (words (em !. "KEYWORDS"))
                  (words inherts)
                  (words (em !. "IUSE"))
                  (words (em !. "CDEPEND"))
                  (words (em !. "PDEPEND"))
                  (em !. "EAPI")
         )

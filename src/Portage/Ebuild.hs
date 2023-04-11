{-# LANGUAGE
    QuasiQuotes
  , UnicodeSyntax
  #-}

module Portage.Ebuild where

import           Hacks

import           Data.List
import qualified Data.Map          as M
import           Data.Text         (pack, replace, unpack)

import qualified System.IO.Strict  as Strict

import           Text.RawString.QQ
import           Text.Regex.TDFA

data Ebuild
  = Ebuild
      { eDepend      :: [String]
      , eRdepend     :: [String]
      , eSlot        :: String
      , eSrc_uri     :: [String]
      , eGit_uri     :: [String]
      , eGit_branch  :: [String]
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

removeJunk ∷ String -> String
removeJunk xs = [ x | x <- xs, x /= '\"' ]

stringSeq ∷ String -> b -> b
stringSeq []      c =  c
stringSeq (_:xs)  c =  stringSeq xs c

-- TODO: make recursive
(!.) ∷ M.Map String String -> String -> String
m !. k =
  let regX = getAllTextSubmatches $ varS =~ [r|\${[^}]*}|] :: [String]
  in case regX of
    []     -> varS
    [v]    -> repl v (m !. (getV v)) varS
    (v:_)  -> repl v (m !. (getV v)) varS -- TODO: handle many env variables
 where varS ∷ String
       varS = maybe "" removeJunk (M.lookup k m)

       repl ∷ String -> String -> String -> String
       repl w ww = unpack . replace (pack w) (pack ww) . pack

       getV :: String -> String
       getV envV = let dropLast = take (length envV - 1) envV
                   in drop 2 dropLast

getEbuild ∷ FilePath -> IO Ebuild
getEbuild f = do
  e <- lines <$> Strict.readFile f
  let em = readStringMap e
      inh = find (`isPrefixOf` "inherit ") e
      inherts = case inh of
                  Just s  -> drop 8 s
                  Nothing -> []
  pure (Ebuild  (words (em !. "DEPEND"))
                (words (em !. "RDEPEND"))
                (em !. "SLOT")
                (words (em !. "SRC_URI"))
                (words (em !. "EGIT_REPO_URI"))
                (words (em !. "EGIT_BRANCH"))
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

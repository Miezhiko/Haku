module Portage.Ebuild where

import           Portage.Types.Package

import           Hacks

import           Data.List
import qualified Data.Map              as M
import           Data.Text             (pack, replace, unpack)

import qualified System.IO.Strict      as Strict

import           Text.Parsec
import           Text.Parsec.String    (Parser)

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
stringSeq []      c = c
stringSeq (_:xs)  c = stringSeq xs c

varParser ∷ Parser String
varParser = do
  _       <- char '$'
  _       <- char '{'
  varName <- many (noneOf "}")
  _       <- char '}'
  pure varName

varsParser ∷ Parser [String]
varsParser = do
  -- Skip characters until a variable is found
  _ <- many (noneOf "${")
  many1 (try varParser)

parseVars ∷ String -> [String]
parseVars s = case parse varsParser "" s of
  Left _     -> []
  Right vars -> vars

repl ∷ String -> String -> String -> String
repl w ww = unpack . replace (pack (setV w)) (pack ww) . pack
 where setV ∷ String -> String
       setV envV = "${" ++ envV ++ "}"

(!.) ∷ (Package, M.Map String String) -> String -> String
(p, m) !. k =
  let varS      = maybe "" id (M.lookup k m)
      vars      = parseVars varS
      replaced  = foldl' foldReplace varS vars
  in case replaced of
    [] -> []
    rp -> removeJunk rp
 where foldReplace ∷ String -> String -> String
       foldReplace str "${PN}" = repl "${PN}" (pName p) str
       foldReplace str "${PV}" = repl "${PV}" "9999" str -- TODO: pass or parse from filename
       foldReplace str var     = repl var ((p, m) !. var) str

getEbuild ∷ Package -> FilePath -> IO Ebuild
getEbuild p f = do
  e <- lines <$> Strict.readFile f
  let emm = readStringMap e
      inh = find (`isPrefixOf` "inherit ") e
      inherts = case inh of
                  Just s  -> drop 8 s
                  Nothing -> []
      em = (p, emm)
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

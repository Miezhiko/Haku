module Commands.Commit
  ( commitCmd
  ) where

import           Types
import           Utils

import           Commands.Digest    (digest)

import           Data.Char          (isDigit)
import           Data.List          (isSuffixOf, maximumBy)
import           Data.Maybe         (mapMaybe)

import           Text.Regex.Posix   ((=~))

import           System.Directory   (getCurrentDirectory, listDirectory)
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import           System.FilePath    (isPathSeparator, takeBaseName, takeDirectory)

splitVersion ∷ String -> [Int]
splitVersion = map read ∘ splitOn '.' ∘ takeWhile (\c -> isDigit c || c == '.')

splitOn ∷ Char -> String -> [String]
splitOn delim s = case dropWhile (== delim) s of
  "" -> []
  s' -> w : splitOn delim s''
    where (w, s'') = break (== delim) s'

compareVersions ∷ String -> String -> Ordering
compareVersions a b = compare (splitVersion a) (splitVersion b)

extractVersion ∷ FilePath -> Maybe String
extractVersion name = 
  case name =~ "([0-9]+(\\.[0-9]+)+)" ∷ [[String]] of
    ((_:_:version:_):_) -> Just version
    _ -> case name =~ "([0-9]+)" ∷ [[String]] of
           ((_:_:version:_):_) -> Just version
           _                   -> Nothing

getHighestVersion ∷ FilePath -> IO (Maybe String)
getHighestVersion dir = do
  files <- listDirectory dir
  let ebuilds = filter (".ebuild" `isSuffixOf`) files
      versions = mapMaybe (extractVersion ∘ takeBaseName) ebuilds
  pure $ if null versions
           then Nothing
           else Just (maximumBy compareVersions versions)

dropTailingPathSeparator :: FilePath -> FilePath
dropTailingPathSeparator path =
  reverse ∘ dropWhile isPathSeparator ∘ reverse $ path

commitEbuild ∷ String -> IO ()
commitEbuild c = do
  let ebName  = takeBaseName c
      catName = takeBaseName $ takeDirectory $ dropTailingPathSeparator c
      catEb   = catName ++ "/" ++ ebName

  highestVersion <- getHighestVersion c
  let commitMsg = case highestVersion of
                    Just ver -> catEb ++ ": " ++ ver
                    Nothing  -> catEb ++ ": "

  digest
  rawAndIgnore "git" [ "add", "." ]
  prevEditor <- lookupEnv "GIT_EDITOR"
  setEnv "GIT_EDITOR" "nano +0,-1 -w"
  rawAndIgnore "git" [ "commit", "-asS", "-m", commitMsg, "-e" ]
  maybe 
    (unsetEnv "GIT_EDITOR") 
    (\ed -> setEnv "GIT_EDITOR" ed)
    prevEditor
 
commit ∷ IO ()
commit = getCurrentDirectory >>= commitEbuild

commitCmd ∷ Command String m
commitCmd = Command { command      = ["commit"]
                    , deps         = []
                    , description  = "commit ebuild changes current directory"
                    , usage        = ("haku " ++)
                    , state        = 𝜀
                    , options      = const 𝜀
                    , handler      = \_ _ -> liftIO commit }

{-# LANGUAGE
    ScopedTypeVariables
  #-}

module Commands.Commit
  ( commitCmd
  ) where

import           Types
import           Utils

import           Commands.Digest    (digest)

import           Control.Exception  (IOException, catch)
import qualified Control.Exception  as Exception

import           Data.Char          (isDigit)
import           Data.List          (isPrefixOf, isSuffixOf, maximumBy)
import           Data.Maybe         (mapMaybe)

import           System.Directory   (doesFileExist, getCurrentDirectory, getTemporaryDirectory,
                                     listDirectory, removeFile)
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import           System.FilePath    (isPathSeparator, takeBaseName, takeDirectory, (</>))
import           System.Process     (callProcess)

splitVersion ∷ String -> [Either Int String]
splitVersion = map classify ∘ splitVersion' ∘ takeWhile isVersionChar
  where
    isVersionChar c = isDigit c || c == '.' || c == '_' || c == '-'
    classify s = if all isDigit s then Left (read s) else Right s

splitVersion' ∷ String -> [String]
splitVersion' s = case dropWhile (== '.') s of
  "" -> []
  s' -> w : splitVersion' s''
    where (w, s'') = break (== '.') s'

compareVersionComponents ∷ Either Int String -> Either Int String -> Ordering
compareVersionComponents (Left a) (Left b)   = compare a b
compareVersionComponents (Right a) (Right b) = compare a b
compareVersionComponents (Left _) (Right _)  = LT  -- Numbers come before strings
compareVersionComponents (Right _) (Left _)  = GT  -- Strings come after numbers

compareVersions ∷ String -> String -> Ordering
compareVersions a b = 
  compareLists (splitVersion a) (splitVersion b)
  where
    compareLists [] [] = EQ
    compareLists [] _  = LT
    compareLists _  [] = GT
    compareLists (x:xs) (y:ys) = 
      case compareVersionComponents x y of
        EQ  -> compareLists xs ys
        res -> res

extractVersion ∷ String -> Maybe String
extractVersion = fmap (drop 1) ∘ stripPrefix "-" ∘ reverse ∘ takeWhile (/= '-') ∘ reverse
  where
    stripPrefix prefix str
      | prefix `isPrefixOf` str = Just (drop (length prefix) str)
      | otherwise = Nothing

getHighestVersion ∷ FilePath -> IO (Maybe String)
getHighestVersion dir = do
  files <- listDirectory dir
  let ebuilds = filter (".ebuild" `isSuffixOf`) files
      versions = mapMaybe (extractVersion ∘takeBaseName) ebuilds
  pure $ if null versions
             then Nothing
             else Just (maximumBy compareVersions versions)

dropTailingPathSeparator ∷ FilePath -> FilePath
dropTailingPathSeparator path =
  reverse ∘ dropWhile isPathSeparator ∘ reverse $ path

checkEmptyMessage ∷ FilePath -> IO Bool
checkEmptyMessage path = do
  exists <- doesFileExist path
  if not exists
    then pure True  -- File was deleted, consider empty
    else do
      content <- readFile path
      let messageLines = filter (not ∘ null) (lines content)  -- Remove empty lines
      pure $ null messageLines || all isCommented messageLines
  where isCommented line = "#" `isPrefixOf` trim line

resetGitCommit ∷ IO ()
resetGitCommit = Exception.handle ignoreResetError $ do
  callProcess "git" ["reset", "--mixed", "HEAD~1"]
  where
    ignoreResetError ∷ IOException -> IO ()
    ignoreResetError _ = pure ()

gitCommitWithEditor ∷ String -> IO ()
gitCommitWithEditor commitMsg = do
  tmpDir <- getTemporaryDirectory
  let tmpPath = tmpDir </> "COMMIT_MSG"

  writeFile tmpPath commitMsg

  Exception.handle ignoreGitAbort $
    callProcess "git" ["commit", "-a", "-S", "-e", "-F", tmpPath]

  shouldAbort <- checkEmptyMessage tmpPath

  when shouldAbort $ do
    putStrLn "Aborting commit due to empty commit message"
    resetGitCommit

  removeFile tmpPath `catch` \(_ ∷ IOException) -> pure ()

 where
  ignoreGitAbort :: Exception.IOException -> IO ()
  ignoreGitAbort _ = putStrLn "Commit aborted"

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
  gitCommitWithEditor $ commitMsg ++ "\n"
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

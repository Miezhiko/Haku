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
import           Data.List          (intercalate, isPrefixOf, isSuffixOf)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (mapMaybe)

import           System.Directory   (doesFileExist, getCurrentDirectory, getTemporaryDirectory,
                                     listDirectory, removeFile)
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import           System.FilePath    (takeBaseName, takeDirectory, takeExtension, (</>))
import           System.Process     (callProcess)

extractVersion ∷ FilePath -> Maybe String
extractVersion filename
  | takeExtension filename /= ".ebuild" = Nothing
  | otherwise =
      let base = takeBaseName filename
          parts = splitOn "-" base
          findVersion [] = Nothing
          findVersion (x:xs)
            | any isDigit x = Just (intercalate "-" (x:xs))
            | otherwise     = findVersion xs
      in findVersion parts

parseVersionSafe ∷ String -> Maybe Version
parseVersionSafe s = case parseVersion s of
    Right v -> Just v
    Left _  -> Nothing

getHighestVersion ∷ FilePath -> IO (Maybe Version)
getHighestVersion dir = do
    files <- listDirectory dir
    let ebuilds = filter (".ebuild" `isSuffixOf`) files
        versions = mapMaybe (extractVersion >=> parseVersionSafe) ebuilds
    pure $ if null versions
           then Nothing
           else Just (maximum versions)

checkEmptyMessage ∷ FilePath -> IO Bool
checkEmptyMessage path = do
  exists <- doesFileExist path
  if not exists
    then pure True  -- File was deleted, consider empty
    else do
      content <- readFile path
      let messageLines = filter (not ∘ null) (lines content)
      pure $ null messageLines || all isCommented messageLines
  where isCommented []   = True
        isCommented line = "#" `isPrefixOf` trim line

resetGitCommit ∷ IO ()
resetGitCommit = Exception.handle ignoreResetError $ do
  callProcess "git" ["reset", "--mixed", "HEAD~1"]
  where ignoreResetError ∷ IOException -> IO ()
        ignoreResetError _ = pure ()

gitCommitWithEditor ∷ String -> IO ()
gitCommitWithEditor commitMsg = do
  tmpDir <- getTemporaryDirectory
  let tmpPath = tmpDir </> "COMMIT_MSG"

  writeFile tmpPath commitMsg

  Exception.handle ignoreGitAbort $
    callProcess "git" ["commit", "-a", "-s", "-S", "-e", "-F", tmpPath]

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
                    Just ver -> catEb ++ ": " ++ showVersion ver
                    Nothing  -> catEb ++ ": "

  digestSuccess <- digest
  when digestSuccess $ do
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

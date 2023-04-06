{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , QuasiQuotes
  , UnicodeSyntax
  #-}

module Commands.Live
  ( liveCmd
  ) where

import           Types
import           Utils

import           Data.Foldable       (for_)
import           Data.List
import           Data.List.Split     (splitOn)
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S

import           Text.RawString.QQ
import           Text.Regex.TDFA

import           System.Console.ANSI
import           System.Directory
import           System.FilePath
import           System.Posix.User   (getRealUserID)

data LiveState
  = LiveState
      { livePreview :: Bool
      , liveForce   :: Bool
      }

liveOpts ∷ Bool -> [OptDescr (LiveState -> LiveState)]
liveOpts _ =
  [ Option "p" ["preview"] (NoArg (\s -> s { livePreview = True })) "check live packages to update"
  , Option "f" ["force"] (NoArg (\s -> s { liveForce = True })) "update all live packages"
  ]

isLiveVersion ∷ Version -> Bool
isLiveVersion (Version [] _ _ _ _)    = False
isLiveVersion (Version [ver] _ _ _ _) = ver == 9999
isLiveVersion (Version xs _ _ _ _)    = 9999 ∈ xs

showLivePackage ∷ (Package, [PackageVersion])
               -> IO ()
showLivePackage (package, vv) = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetUnderlining SingleUnderline ]
  print package
  setSGR [ SetColor Foreground Vivid Green
         , SetUnderlining NoUnderline
         , SetConsoleIntensity BoldIntensity ]
  putStrLn $ prettyShowVersionsList vv
  setSGR [ Reset ]
  putStrLn []

{- HLINT ignore "Redundant <$>" -}
liveRebuild ∷ [(Package, [PackageVersion])]
           -> IO ()
liveRebuild pvv = (== 0) <$> getRealUserID >>= \root ->
  if root
    then rawAndIgnore "emerge" ("-av" : packagesList)
    else hasSudo $ rawAndIgnore "sudo" ("emerge" : ("-av" : packagesList))
 where getOneVersion ∷ [PackageVersion] -> String
       getOneVersion []    = []
       getOneVersion [pv]  = show (pvVersion pv)
       getOneVersion (x:_) = getOneVersion [x]

       packagesList ∷ [String]
       packagesList = map (\(p, vv) ->
                            case getOneVersion vv of
                             [] -> show p
                             sv -> "=" ++ show p ++ "-" ++ sv
                          ) pvv

isThereGitUpdates ∷ String -> String -> String -> [String] -> IO Bool
isThereGitUpdates  repo repoFilePath rName mbBranch = do
  putStrLn $ "Checking for: " ++ repo
  rlm <- case mbBranch of
    []     -> readIfSucc "git" ["ls-remote", repo, "HEAD"]
    (rb:_) -> readIfSucc "git" ["ls-remote", repo, rb]
  case rlm of
    Nothing  -> pure True
    Just rlc -> do
      let remoteHash = head (splitOn "\t" rlc)
      repoHashFile <- readFile repoFilePath
      let currentHash = head (splitOn "\t" repoHashFile)
      if currentHash == remoteHash
        then do putStrLn $ rName ++ " is up to date, hash: " ++ currentHash
                pure False
        else do putStrLn $ rName ++ " will be updated to: " ++ remoteHash
                pure True

checkForRepository ∷ PortageConfig
                  -> (Package, [PackageVersion])
                  -> String
                  -> [String]
                  -> IO (Maybe (Package, [PackageVersion]))
checkForRepository pc (package, liveVersions) repo mbBranch = do
  let rx = getAllTextSubmatches $ repo =~ [r|https://github.com/([^/]+)/([^/]+)(.git)?|] :: [String]
      rOwner          = rx !! 1
      rNameGit        = rx !! 2
      -- TODO: this is because I can't write proper regex in POSIX style
      rName           = if ".git" `isSuffixOf` rNameGit
                          then take (length rNameGit - 4) rNameGit
                          else rNameGit
      repoPath        = treePath </> "distfiles/git3-src" </> rOwner ++ "_" ++ rName ++ ".git"
      repoFilePath    = repoPath </> "FETCH_HEAD"
  doesFileExist repoFilePath >>=
      \case False -> pure $ Just (package, liveVersions) 
            True  -> do
              gitUpdates <- isThereGitUpdates repo repoFilePath rName mbBranch
              if gitUpdates
                then pure $ Just (package, liveVersions) 
                else pure Nothing
 where treePath ∷ String
       treePath = pcMakeConf pc M.! "PORTDIR"

smartLiveRebuild ∷ PortageConfig
                -> Package
                -> [PackageVersion]
                -> IO (Maybe (Package, [PackageVersion]))
smartLiveRebuild _ _ []             = pure Nothing
smartLiveRebuild pc package (ver:_) = -- TODO: many versions
  findVersionedEbuild pc package ver >>=
    \case Nothing -> pure $ Just (package, [ver])
          Just eb ->
            case eGit_uri eb of
              []     -> pure $ Just (package, [ver]) -- not git?
              (rp:_) -> checkForRepository pc (package, [ver])
                                           rp (eGit_branch eb)

liveUpdateMap ∷ Package
             -> PortageConfig
             -> LiveState
             -> IO (Maybe (Package, [PackageVersion]))
liveUpdateMap package pc lss =
  let versions      = pVersions package
      versionsList  = S.toList versions
      installed     = filter pvInstalled versionsList
  in if null installed
    then pure Nothing
    else let liveVersions = filter (isLiveVersion . pvVersion) installed
          in if null liveVersions
              then pure Nothing
              else if liveForce lss
                    then pure $ Just (package, liveVersions)
                    else smartLiveRebuild pc package liveVersions

liveUpdateIO ∷ IORef PortageConfig -> LiveState -> [String] -> IO ()
liveUpdateIO rpc lss filterPackages = readIORef rpc >>= \pc -> do
  llst <- traverse (\(a, package) ->
        case filterPackages of
          [] -> liveUpdateMap package pc lss
          xs -> if any (`isInfixOf` a) xs
                  then liveUpdateMap package pc lss
                  else pure Nothing
        ) (M.toList (pcTree pc))
  case catMaybes llst of
    [] -> putStrLn "No live packages found"
    xs -> if livePreview lss
            then for_ xs showLivePackage
            else liveRebuild xs

liveUpdate ∷ HakuMonad m ⇒ LiveState -> [String] -> m ()
liveUpdate lss xs = ask >>= \env ->
   liftIO $ liveUpdateIO (config env) lss xs

liveCmd ∷ Command LiveState m
liveCmd = Command
            { command = ["live"]
            , description = "Live rebuild"
            , usage = ("haku " ++)
            , state = LiveState { livePreview    = False
                                , liveForce  = False }
            , options = liveOpts
            , handler = liveUpdate }

{-# LANGUAGE
    OverloadedStrings
  #-}

module Commands.Live
  ( liveCmd
  ) where

import           Types
import           Utils

import           Portage.Config      (resolveDistDir)

import           Data.Foldable       (for_)
import           Data.List
import           Data.List.Split     (splitOn)
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S

import           Text.Parsec
import           Text.Parsec.String  (Parser)

import           System.Console.ANSI
import           System.Directory
import           System.FilePath

data RepoInfo
  = RepoInfo
      { _host     :: String
      , _owner    :: String
      , _repoName :: String
      }

data LiveState
  = LiveState
      { liveVerbose  :: Bool
      , livePreview  :: Bool
      , liveForce    :: Bool
      , liveDownload :: Bool
      , liveAsk      :: Bool
      }

repoParser ∷ Parser RepoInfo
repoParser = do
  _         <- string "https://"
  hostName  <- many1 (noneOf "/")
  _         <- string "/"
  ownerName <- many1 (noneOf "/")
  _         <- char '/'
  repoName  <- manyTill anyChar (try (string ".git") <|> lookAhead (string "/")
                                                     <|> ("" <$ eof))
  _         <- optional (string ".git")
  return $ RepoInfo hostName ownerName repoName

liveOpts ∷ Bool -> [OptDescr (LiveState -> LiveState)]
liveOpts _ =
  [ Option "v" ["verbose"]  (NoArg (\s -> s { liveVerbose = True }))  "verbose output" 
  , Option "p" ["preview"]  (NoArg (\s -> s { livePreview = True }))  "check live packages to update"
  , Option "f" ["force"]    (NoArg (\s -> s { liveForce = True }))    "update all live packages"
  , Option "d" ["download"] (NoArg (\s -> s { liveDownload = True })) "download not downloaded"
  , Option "a" ["ask"]      (NoArg (\s -> s { liveAsk = True }))      "ask before update"
  ]

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

liveRebuild ∷ [(Package, [PackageVersion])]
           -> Bool
           -> IO ()
liveRebuild pvv a = isRoot (rawAndIgnore "emerge" opts)
                           (rawAndIgnore "sudo" ("emerge" : opts))
 where opts ∷ [String]
       opts = "-v" : (["-a" | a] ++ packagesList)

       getOneVersion ∷ [PackageVersion] -> String
       getOneVersion []     = []
       getOneVersion (pv:_) = show (pvVersion pv)

       packagesList ∷ [String]
       packagesList = map (\(p, vv) ->
                            case getOneVersion vv of
                             [] -> show p
                             sv -> "=" ++ show p ++ "-" ++ sv
                          ) pvv

checkForRemoteRepositoryHash ∷ String -> [String] -> IO (Maybe String)
checkForRemoteRepositoryHash repo [] = do
  setSGR [ SetColor Foreground Vivid Blue
         , SetConsoleIntensity BoldIntensity ]
  putStrLn $ "Checking for: " ++ repo
  setSGR [ Reset ]
  readIfSucc "git" ["ls-remote", repo, "HEAD"]
checkForRemoteRepositoryHash repo [rb] = do
  setSGR [ SetColor Foreground Vivid Cyan
         , SetConsoleIntensity BoldIntensity ]
  putStrLn $ "Checking for: " ++ repo ++ " branch " ++ rb
  setSGR [ Reset ]
  readIfSucc "git" ["ls-remote", repo, rb]
checkForRemoteRepositoryHash repo (x:xs) =
  checkForRemoteRepositoryHash repo [x] >>=
  \case Nothing -> checkForRemoteRepositoryHash repo xs
        Just rl -> pure $ Just rl

isThereGitUpdates ∷ String -> String -> String -> [String] -> IO Bool
isThereGitUpdates repo repoFilePath rName mbBranch =
  checkForRemoteRepositoryHash repo mbBranch >>=
  \case Nothing  -> pure True
        Just rlc -> do
          let rlcParts   = splitOn "\t" rlc
          let remoteHash = case rlcParts of
                            (h : _) -> h
                            []      -> []
          repoHashFile <- readFile repoFilePath
          let repoHashParts = splitOn "\t" repoHashFile
              currentHash   = case repoHashParts of
                                (h : _) -> h
                                []      -> []
          if currentHash == remoteHash
            then do putStrLn $ rName ++ " is up to date, hash: " ++ currentHash
                    pure False
            else do putStrLn $ rName ++ " will be updated to: " ++ remoteHash
                    pure True

checkForRepository' ∷ String -- repo owner
                   -> String -- repo name
                   -> (Package, [PackageVersion])
                   -> (String, [String])
                   -> FilePath -- DISTDIR
                   -> Bool     -- download
                   -> IO (Maybe (Package, [PackageVersion]))
checkForRepository' rOwner rName (p, lv) (repo, mbBranch) distDir dl =
  let repoPath        = distDir </> "git3-src" </> rOwner ++ "_" ++ rName ++ ".git"
      repoFilePath    = repoPath </> "FETCH_HEAD"
  in doesFileExist repoFilePath >>=
      \case False -> do putStrLn $ show p ++ ": not downloaded on " ++ repoFilePath
                        if dl then pure $ Just (p, lv) 
                              else pure Nothing
            True  -> do
              gitUpdates <- isThereGitUpdates repo repoFilePath rName mbBranch
              if gitUpdates
                then pure $ Just (p, lv) 
                else pure Nothing

checkForRepository ∷ (Package, [PackageVersion])
                  -> String
                  -> [String]
                  -> FilePath
                  -> Bool     -- download
                  -> IO (Maybe (Package, [PackageVersion]))
checkForRepository (p, lv) repo mbBranch distDir dl = do
  case parse repoParser "" repo of
    Right (RepoInfo _ repoOwner repoName) ->
      checkForRepository' repoOwner repoName (p, lv) (repo, mbBranch) distDir dl
    Left _ -> do
      putStrLn $ show p ++ ": ERROR ON PARSING: " ++ repo
      pure Nothing

smartLiveRebuild ∷ PortageConfig
                -> FilePath         -- DISTDIR
                -> Package
                -> [PackageVersion]
                -> Bool             -- verbose
                -> Bool             -- download
                -> IO (Maybe (Package, [PackageVersion]))
smartLiveRebuild _ _ _ [] _ _                          = pure Nothing
smartLiveRebuild pc distDir package (ver:_) verbose dl = -- TODO: many versions
  findVersionedEbuild pc package ver >>=
    \case Nothing -> do putStrLn $ show package ++ ": Can't find ebuild"
                        pure $ Just (package, [ver])
          Just eb ->
            case eGit_commit eb of
              (_:_)  -> do when verbose $ putStrLn $ show package ++ ": EGIT_COMMIT is set, skipping"
                           pure Nothing
              []     -> case eGit_uri eb of
                          []     -> do when verbose $ putStrLn $ show package ++ ": Can't find EGIT_SRC"
                                       pure Nothing -- not git?
                          [rp]   -> checkForRepository (package, [ver])
                                                       rp (eGit_branch eb)
                                                       distDir dl
                          xs     ->
                            filterM (\rp ->
                              case parse repoParser "" rp of
                                Right (RepoInfo _ rOwner rName) ->
                                  let repoPath     = distDir </> "git3-src"
                                                             </> rOwner ++ "_" ++ rName ++ ".git"
                                      repoFilePath = repoPath </> "FETCH_HEAD"
                                  in doesFileExist repoFilePath
                                Left _ -> pure False) xs
                            >>= \case
                              []     -> do putStrLn $ show package ++ ": There are several EGIT_SRC but none downloaded"
                                           pure $ Just (package, [ver])
                              (rp:_) -> checkForRepository (package, [ver])
                                                           rp (eGit_branch eb)
                                                           distDir dl

liveUpdateMap ∷ FilePath -- DISTDIR
             -> Package
             -> PortageConfig
             -> LiveState
             -> IO (Maybe (Package, [PackageVersion]))
liveUpdateMap distDir package pc lss =
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
                    else smartLiveRebuild pc distDir package
                                          liveVersions
                                          (liveVerbose lss)
                                          (liveDownload lss)

liveUpdateIO ∷ IORef PortageConfig
            -> (String -> IO ())
            -> LiveState
            -> [String]
            -> IO ()
liveUpdateIO rpc hlog lss filterPackages = readIORef rpc >>= \pc -> do
  distDir <- resolveDistDir pc
  llst <- traverse (\(a, package) ->
        case filterPackages of
          [] -> liveUpdateMap distDir package pc lss
          xs -> if any (`isInfixOf` a) xs
                  then liveUpdateMap distDir package pc lss
                  else pure Nothing
        ) (M.toList (pcTree pc))
  case catMaybes llst of
    [] -> hlog "<Yellow>No live packages found"
    xs -> if livePreview lss
            then do
              putStrLn []
              for_ xs showLivePackage
            else liveRebuild xs $ liveAsk lss

liveUpdate ∷ LiveState ~> m
liveUpdate lss xs = ask >>= \env ->
   liftIO $ liveUpdateIO (config env) (logger env) lss xs

liveCmd ∷ Command LiveState m
liveCmd = Command
            { command     = ["live"]
            , deps        = [PortageMeta, OverlayMeta, MiscMeta]
            , description = "Live rebuild"
            , usage       = ("haku " ++)
            , state       = LiveState { liveVerbose   = False
                                      , livePreview   = False
                                      , liveForce     = False
                                      , liveDownload  = False
                                      , liveAsk       = False }
            , options     = liveOpts
            , handler     = liveUpdate }

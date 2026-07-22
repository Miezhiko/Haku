module Portage.Config
  ( module Portage.Types.Config
  , loadPortageConfig
  , portageConfig
  , resolveDistDir
  , resolvePortageTmpDir
  , restoreConfig
  , storeConfig
  ) where

import           Console
import           Constants
import           Env
import           Hacks

import           Portage.Mask
import           Portage.Types.Config
import           Portage.Types.Package
import           Portage.Version
import           Shelter.Hashes

import           Data.Binary
import qualified Data.ByteString.Lazy  as BL
import           Data.Function
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Ord              (comparing)
import qualified Data.Set              as S
import           Data.Time.Clock

import           System.Directory
import           System.FilePath
import qualified System.IO.Strict      as Strict
import           System.Process

import           Control.Exception     (SomeException, try)
import           Control.Monad

import           UnliftIO.Async


parseEnvMap ∷ String -> ConfData
parseEnvMap s = M.fromList $ map parseLine (lines s)
 where
  parseLine l = 
    case break (=='=') l of
      (v, '=':c) -> (v, stripQuotes c)
      (v, [])    -> (v, [])
      (_, p:_)   -> error $ "Unexpected character: " ++ [p]

  stripQuotes ('\'':r) | not (null r) = init r
  stripQuotes x                       = x

getFilteredDirectoryContents ∷ FilePath -> IO [FilePath]
getFilteredDirectoryContents fp = filter (∉ [".",".."]) <$> getDirectoryContents fp

getConfigFile ∷ FilePath -> IO ConfData
getConfigFile f = do
  (_,r,_) <- readCreateProcessWithExitCode (
    shell $ "unset $(set | sed 's/^\\([^=]*\\)=.*$/\\1/') 2>/dev/null;" ++
    "source " ++ f ++ "; set" ) []
  pure $ parseEnvMap r

-- | Ask portageq, since a lot of stock installs never write these
--   variables into make.conf and just rely on Portage's own defaults
--   (which can also depend on repos.conf, not just make.conf).
queryPortageq ∷ [String] -> IO (Maybe String)
queryPortageq args = do
  result ← try (readProcess "portageq" args []) ∷ IO (Either SomeException String)
  pure $ case result of
    Left  _   -> Nothing
    Right out -> case rstrip out of
                  [] -> Nothing
                  xs -> Just xs

-- | Look a variable up in make.conf, falling back to `portageq envvar`
--   when it isn't explicitly set.
resolveConfVar ∷ ConfData -> String -> IO (Maybe String)
resolveConfVar makeConf var =
  case M.lookup var makeConf of
    v@(Just _) -> pure v
    Nothing    -> queryPortageq ["envvar", var]

resolveDistDir ∷ PortageConfig -> IO FilePath
resolveDistDir pc = resolveConfVar (pcMakeConf pc) "DISTDIR" >>=
  \case Just d  -> pure d
        Nothing -> error "Haku: can't determine DISTDIR (set it in make.conf or ensure portageq works)"

resolvePortageTmpDir ∷ PortageConfig -> IO FilePath
resolvePortageTmpDir pc = resolveConfVar (pcMakeConf pc) "PORTAGE_TMPDIR" >>=
  \case Just d  -> pure d
        Nothing -> error "Haku: can't determine PORTAGE_TMPDIR (set it in make.conf or ensure portageq works)"

-- | The main repo path (legacy PORTDIR), falling back to portageq when
--   it isn't set explicitly -- most modern installs configure it only
--   via repos.conf and never touch PORTDIR at all.
resolveMainRepoPath ∷ ConfData -> IO FilePath
resolveMainRepoPath makeConf =
  case M.lookup "PORTDIR" makeConf of
    Just p  -> pure p
    Nothing -> queryPortageq ["get_repo_path", "/", "gentoo"] >>=
      \case Just p  -> pure p
            Nothing -> error "Haku: can't determine main repo path (set PORTDIR in make.conf or ensure portageq works)"

-- | Overlay/extra-repo paths (legacy PORTDIR_OVERLAY), falling back to
--   every repo portageq knows about besides the main one.
resolveOverlayPaths ∷ ConfData -> FilePath -> IO [FilePath]
resolveOverlayPaths makeConf mainRepoPath =
  case M.lookup "PORTDIR_OVERLAY" makeConf of
    Just o  -> pure $ filter (not ∘ null) $ splitOnAnyOf ["\\n","\\t"," ","'","$"] o
    Nothing -> queryPortageq ["get_repos", "/"] >>=
      \case Nothing -> pure 𝜀
            Just rs -> do
              let repoNames = filter (≠ "gentoo") (words rs)
              paths <- traverse (\r -> queryPortageq ["get_repo_path", "/", r]) repoNames
              pure [ p | Just p <- paths, p ≠ mainRepoPath ]

getVersions ∷ String -> String -> String -> IO (S.Set PackageVersion)
getVersions fp pn o =
  liftM (S.fromList ∘ map (getVersion o pn) ∘ filter (isSuffixOf ".ebuild"))
        (getFilteredDirectoryContents fp)

parseOverlay ∷ FilePath -> IO (([Package], [String]), OverlayMeta)
parseOverlay treePath = do
  (overlayName, profilesMask) <- concurrently
    (rstrip <$> readFile (treePath </> constProfilesRepoName))
    (do
      let profilesMaskFile = treePath </> constProfilesPackageMask
      pmfExists <- doesFileExist profilesMaskFile
      if pmfExists
        then parseMask . rstrip <$> readFile profilesMaskFile
        else pure []
    )

  treeCats <- getFilteredDirectoryContents treePath
  let ecls = "eclass" `elem` treeCats
  eclasses <- if ecls
    then getFilteredDirectoryContents (treePath </> "eclass")
    else pure []

  let filteredCats = filter (`notElem` [".git","eclass","metadata","profiles"]) treeCats
  catMap <- forConcurrently filteredCats $ \cat -> do
    let fcat = treePath </> cat
    isDir <- doesDirectoryExist fcat
    if not isDir
      then pure ([], (cat, []))
      else do
        packages <- getFilteredDirectoryContents fcat
        let filteredPackages = filter (/= "metadata.xml") packages
        pkgs <- forConcurrently filteredPackages $ \pn -> do
          let fp = fcat </> pn
          isPkgDir <- doesDirectoryExist fp
          if not isPkgDir
            then pure Nothing
            else do
              versions <- getVersions fp pn overlayName
              pure $ Just (Package cat versions pn)
        pure (catMaybes pkgs, (cat, filteredPackages))

  let pkgs        = concatMap fst catMap
      cpkg        = map snd catMap
      overlayMeta = (overlayName, OverlayData treePath cpkg profilesMask)

  pure ((pkgs, eclasses), overlayMeta)

parseOverlays ∷ [FilePath] -> IO (Tree, [OverlayMeta])
parseOverlays overlys = do
  parsed <- traverse parseOverlay overlys
  let treePkgs  = concatMap (fst ∘ fst) parsed
      trees     = map (\p -> (pCategory p </> pName p, p)) treePkgs
      ovMetas   = map snd parsed
  pure (M.fromList trees, ovMetas)

mergePackages ∷ Package -> Package -> Package
mergePackages p1 p2 = p1 { pVersions = versions }
  where versions = S.union (pVersions p1) (pVersions p2)

findExactMax ∷ [String] -> Maybe String
findExactMax [ ] = Nothing
findExactMax [x] = Just x
findExactMax xss = Just (maximumBy (comparing length) xss)

getPackage ∷ String -> String -> Maybe String -> String -> IO (Maybe (Atom, Package))
getPackage _ _ Nothing _ = pure Nothing
getPackage fcat cat (Just pn) vp = do
  overlay <- rstrip <$> Strict.readFile (fcat </> vp </> "repository")
  case getVersionInstalled overlay pn vp of
    Left err -> putStrLn err
             >> pure Nothing
    Right vi -> pure ∘ Just $ (cat </> pn, Package cat (S.singleton vi) pn)

concatPackageGroups ∷ [(Atom, Package)] -> [(Atom, Package)]
concatPackageGroups [] = 𝜀
concatPackageGroups [(a, p)] = [(a, p)]
concatPackageGroups xs@(headTuple : _) =
  let anyAtom   = fst headTuple
      firstPkg  = snd headTuple
      anyCat    = pCategory firstPkg
      anyName   = pName firstPkg
      versions  = S.fromList (S.toList ∘ pVersions ∘ snd =<< xs)
  in [(anyAtom, Package anyCat versions anyName)]

findPackages ∷ String -> String -> [String] -> [String] -> IO Tree
findPackages fcat cat versionedPkgs pkgs = do
  results <- mapM (\vpkg ->
    let mb = filter (`isPrefixOf` vpkg) pkgs
    in getPackage' (findExactMax mb) vpkg) versionedPkgs
  let sorted  = sortOn fst (catMaybes results)
      grouped = groupBy ((==) `on` fst) sorted
      gmp     = concatMap concatPackageGroups grouped
  pure $ M.fromList gmp
 where getPackage' = getPackage fcat cat

getInstalledPackages ∷ FilePath -> [(String, [String])] -> IO Tree
getInstalledPackages pkgdb categories = do
  let catMap = M.fromList categories
  treeCats <- getFilteredDirectoryContents pkgdb
  catMaps <- forConcurrently treeCats $ \cat -> do
    let fcat = pkgdb </> cat
    isDir <- doesDirectoryExist fcat
    if not isDir
      then pure M.empty
      else case M.lookup cat catMap of
        Just pkgs -> do
          pkgFiles <- getFilteredDirectoryContents fcat
          findPackages fcat cat pkgFiles pkgs
        Nothing -> pure M.empty
  pure $ foldl' (M.unionWith mergePackages) M.empty catMaps

loadPortageConfig ∷ [MetaData] -> IO PortageConfig
loadPortageConfig [] = -- provide empty config for no metadata request
  pure $ PortageConfig M.empty [] [] M.empty
                       M.empty (MaskingData [] [])
                       M.empty False -- update cahce
loadPortageConfig _meta = do
  progressThread <- startProgress "Regenerating Haku cache..."

  makeConf <- getConfigFile constMakeConfPath
  treePath <- resolveMainRepoPath makeConf

  ((catMap, eclasses), (treeName, treeData)) <- parseOverlay treePath

  portageMask <- doesFileExist constPortagePackageMask >>= \pmfExists ->
    if pmfExists
      then do fileData <- rstrip <$> readFile constPortagePackageMask
              pure $ parseMask fileData
      else pure []

  overlayPaths <- resolveOverlayPaths makeConf treePath
  (ov, met) <- if null overlayPaths
                then pure (M.empty, 𝜀)
                else parseOverlays overlayPaths

  let atoms       = map (\p -> (pCategory p </> pName p, p)) catMap
      pkgs        = M.fromList atoms
      merged      = M.unionWith mergePackages pkgs ov
      metaList    = (treeName, treeData) : met
      maskData    = MaskingData
                      portageMask
                      [] -- for keywords

      overlays    = M.fromList metaList

      categories  = map groupCat ∘ groupBy ((==) `on` fst)
                                 ∘ sortBy (comparing fst)
                                 ∘ concatMap (\(_, oDt) -> ovCategories oDt)
                                 $ metaList
                      where groupCat []       = error "Empty Group"
                            groupCat xs@(x:_) = (fst x, concatMap snd xs)

  installed     <- getInstalledPackages constInstalledPath categories
  shelterHashes <- getShelterHashes

  let finalTree = M.unionWith mergePackages installed merged

  finishProgress progressThread

  pure $ PortageConfig makeConf
                       categories
                       eclasses
                       finalTree
                       overlays
                       maskData
                       shelterHashes
                       True -- update cahce

storeConfig ∷ Handle -> PortageConfig -> IO ()
storeConfig h = BL.hPut h ∘ encode

restoreConfig ∷ [MetaData] -> Handle -> IO PortageConfig
restoreConfig meta h = BL.hGetContents h >>= \c ->
  case decodeOrFail c of
    Left (_,_,err) -> do putStrLn $ "Failed to decode cache: " ++ err
                         loadPortageConfig meta
    Right (_,_,pc) -> pure pc

updateWithMaybeShelter ∷ [MetaData]
                      -> Maybe ShelterConfig
                      -> PortageConfig
                      -> IO PortageConfig
updateWithMaybeShelter _ (Just shelter) binaryParsedConfig
  | isPortageConfigIsInSync binaryParsedConfig shelter
    = pure $ binaryParsedConfig { pcUpdateCache = False }
updateWithMaybeShelter meta _ _ = loadPortageConfig meta

maybeUpdateConfig ∷ [MetaData] -> Handle -> IO PortageConfig
maybeUpdateConfig meta h = getShelterConfig >>= \shelter ->
  restoreConfig meta h >>= updateWithMaybeShelter meta shelter

portageConfig ∷ [MetaData]
             -> FilePath
             -> Handle
             -> IO PortageConfig
portageConfig [] _ hakuCacheHandle = restoreConfig [] hakuCacheHandle
                                      >>= \pc -> pure pc { pcUpdateCache = False }
portageConfig meta hakuCachePath hakuCacheHandle = do
  -- if cache is more than one minute old recheck if
  -- shelter hashes changed (update was made and was meaningful)
  currentTime <- getCurrentTime
  -- getModificationTime returns UTC time (as it's in type)
  changemTime <- getModificationTime hakuCachePath
  -- conversion functions will treat it as seconds
  let diff = diffUTCTime currentTime changemTime
  if diff > 60 ∧ UpdateMeta ∈ meta
    then maybeUpdateConfig meta hakuCacheHandle
    else portageConfig [] hakuCachePath hakuCacheHandle

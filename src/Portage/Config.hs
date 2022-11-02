{-# LANGUAGE UnicodeSyntax #-}
module Portage.Config
  ( module Portage.Types.Config
  , portageConfig
  , restoreConfig
  , storeConfig
  ) where

import           Constants

import           Portage.Helper
import           Portage.Types.Config
import           Portage.Types.Package
import           Portage.Version

import           Prelude.Unicode

import           Data.Binary
import qualified Data.ByteString.Lazy  as BL
import           Data.Function
import           Data.Functor
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Ord              (comparing)
import qualified Data.Set              as S
import           Data.Time.Clock

import           System.Directory
import           System.FilePath
import qualified System.IO.Strict      as Strict
import           System.Posix.Files
import           System.Process

import           Control.Arrow
import           Control.Monad

parseEnvMap ∷ String → EnvMap
parseEnvMap s = M.fromList $
                   [  (v,stripQuotes c) | 
                      l <- lines s,
                      (v,'=':c) <- return $ break (=='=') l ]
  where  stripQuotes ('\'':r@(_:_)) =  init r
         stripQuotes x              =  x

getFilteredDirectoryContents ∷ FilePath → IO [FilePath]
getFilteredDirectoryContents fp = filter (∉ [".",".."]) <$> getDirectoryContents fp

getConfigFile ∷ FilePath → IO EnvMap
getConfigFile f =  do  (_,r,_) <- readCreateProcessWithExitCode (
                                    shell $  "unset $(set | sed 's/^\\([^=]*\\)=.*$/\\1/') 2>/dev/null;" ++
                                             "source " ++ f ++ "; set" ) []
                       return (parseEnvMap r)

getVersions ∷ String → String → String → IO (S.Set PackageVersion)
getVersions fp pn o = do
  dirContent <- getFilteredDirectoryContents fp
  let ebuilds   = filter (isSuffixOf ".ebuild") dirContent
      versions  = map (getVersion o pn) ebuilds
  return $ S.fromList versions

parseOverlay ∷ FilePath → IO ([Package], OverlayMeta)
parseOverlay treePath = do
  overlayName  <- rstrip <$> readFile (treePath </> "profiles/repo_name")
  treeCats     <- getFilteredDirectoryContents treePath
  filteredCats <- filterM (\(f, _) -> getFileStatus f <&> isDirectory)
                      $ map (\c -> (treePath </> c, c))
                            (filter (∉ [".git","eclass","metadata","profiles"]) treeCats)
  catMap <- mapM (\(fcat, cat) -> do
                    packages <- getFilteredDirectoryContents fcat
                    packagesFiltered <- filterM (\(fp, _) -> getFileStatus fp <&> isDirectory)
                                            $ map (\p -> (fcat </> p, p))
                                                  (filter (∉ ["metadata.xml"]) packages)
                    parsed <- mapM (\(fp, pn) -> do
                                versions <- getVersions fp pn overlayName
                                return $ Package cat versions pn
                              ) packagesFiltered
                    return (parsed, (cat, map snd packagesFiltered))
                 ) filteredCats -- [[package], a]
  let pkgs = concatMap fst catMap
      cpkg = map snd catMap
  return (pkgs, (overlayName, (treePath, cpkg)))

parseOverlays ∷ String → IO (Tree, [OverlayMeta])
parseOverlays input = do
  let overlys = filter (not ∘ null) $ splitOnAnyOf ["\\n","\\t"," ","'","$"] input
  parsed <- mapM parseOverlay overlys
  let treePkgs  = concatMap fst parsed
      trees     = map (\p -> (pCategory p ++ "/" ++ pName p, p)) treePkgs
      ovMetas   = map snd parsed
  return (M.fromList trees, ovMetas)

mergePackages ∷ Package → Package → Package
mergePackages p1 p2 =
  let versions  = S.union (pVersions p1) (pVersions p2)
  in Package (pCategory p1) versions (pName p1)

findExactMax ∷ [String] → Maybe String
findExactMax []  = Nothing
findExactMax [x] = Just x
findExactMax xss = Just (maximumBy (comparing length) xss)

getPackage ∷ String → String → Maybe String → String → IO (Maybe (Atom, Package))
getPackage _ _ Nothing _ = return Nothing
getPackage fcat cat (Just pn) vp = do
  overlay <- rstrip <$> Strict.readFile (fcat </> vp </> "repository")
  let versions = S.singleton (getVersionInstalled overlay pn vp)
  return $ Just (cat ++ "/" ++ pn, Package cat versions pn)

concatPackageGroups ∷ [(Atom, Package)] → [(Atom, Package)]
concatPackageGroups [] = 𝜀
concatPackageGroups [(a, p)] = [(a, p)]
concatPackageGroups xs =
  let headTuple = head xs
      anyAtom   = fst headTuple
      firstPkg  = snd headTuple
      anyCat    = pCategory firstPkg
      anyName   = pName firstPkg
      versions  = S.fromList $ concatMap (\(_, pp) -> S.toList (pVersions pp)) xs
  in [(anyAtom, Package anyCat versions anyName)]

findPackages ∷ String → String → [String] → [String] → IO Tree
findPackages fcat cat versionedPkgs pkgs = do
  l <- mapM (\vpkg -> let mb = filter (`isPrefixOf` vpkg) pkgs
                      in getPackage fcat cat (findExactMax mb) vpkg
            ) versionedPkgs
  let srt = sortBy (\(a, _) (b, _) -> compare a b) (catMaybes l)
      grp = groupBy (\(a, _) (b, _) -> a == b) srt
      gmp = concatMap concatPackageGroups grp
  return $ M.fromList gmp

concatMaps ∷ Tree → [Tree] → Tree
concatMaps base []     = base
concatMaps base [x]    = M.unionWith mergePackages base x
concatMaps base (x:xs) = M.unionWith mergePackages (concatMaps base [x])
                                                   (concatMaps base xs)

getInstalledPackages ∷ FilePath → [(String, [String])] → IO Tree
getInstalledPackages pkgdb categories = do
  treeCats      <- getFilteredDirectoryContents pkgdb
  filteredCats  <- filterM (\(f, _) -> getFileStatus f <&> isDirectory)
                      $ map (\c -> (pkgdb </> c, c))
                            treeCats
  catMaps <- mapM (\(fcat, cat) -> do
                    pkgFiles <- getFilteredDirectoryContents fcat
                    let myCat = find (\(c, _) -> cat == c
                                     ) categories
                    case myCat of
                      Just (_,pkgs) -> findPackages fcat cat pkgFiles pkgs
                      Nothing       -> return M.empty
                  ) filteredCats
  return $ concatMaps M.empty catMaps

storeConfig ∷ PortageConfig → IO ()
storeConfig = BL.writeFile constHakuCache ∘ encode

restoreConfig ∷ IO PortageConfig
restoreConfig = decode <$> BL.readFile constHakuCache

loadPortageConfig ∷ IO PortageConfig
loadPortageConfig = do
  makeConf <- getConfigFile constMakeConfPath
  let treePath = makeConf M.! "PORTDIR"

  (catMap, (ovName, (_, categoriesMain))) <- parseOverlay treePath

  (ov, met) <- case M.lookup "PORTDIR_OVERLAY" makeConf of
                  Just o  -> parseOverlays o
                  Nothing -> return (M.empty, 𝜀)

  let atoms       = map (\p -> (pCategory p ++ "/" ++ pName p, p)) catMap
      pkgs        = M.fromList atoms
      merged      = M.unionWith mergePackages pkgs ov
      metaList    = (ovName, (treePath, categoriesMain)) : met
      overlays    = M.fromList metaList
      categories  = map (fst ∘ head &&& concatMap snd) 
                      ∘ groupBy ((==) `on` fst)
                      ∘ sortBy (comparing fst)
                      ∘ concatMap (snd ∘ snd)
                      $ metaList

  installed <- getInstalledPackages constInstalledPath categories
  let finalTree = M.unionWith mergePackages installed merged
      config = PortageConfig makeConf categories finalTree overlays

  storeConfig config

  return config

portageConfig ∷ IO PortageConfig
portageConfig = do
  cacheExists <- doesFileExist constHakuCache
  if cacheExists
    then do
      -- todo: maybe some other checks to see if tree should be updated
      currentTime <- getCurrentTime
      changeTime <- getModificationTime constHakuCache
      let diff = diffUTCTime currentTime changeTime
      if diff > (10 × 60) -- Conversion functions will treat it as seconds
        then loadPortageConfig
        else restoreConfig
    else loadPortageConfig

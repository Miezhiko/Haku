{-# LANGUAGE UnicodeSyntax #-}
module Portage.Config
  ( module Portage.Types.Config
  , portageConfig
  ) where

import           Portage.Package
import           Portage.Types.Config
import           Portage.Version

import           Data.Functor
import           Data.List
import           Data.List.Split
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S

import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Control.Monad

parseEnvMap ∷ String → EnvMap
parseEnvMap s = M.fromList $
                   [  (v,stripQuotes c) | 
                      l <- lines s,
                      (v,'=':c) <- return $ break (=='=') l ]
  where  stripQuotes ('\'':r@(_:_)) =  init r
         stripQuotes x              =  x

getFilteredDirectoryContents ∷ FilePath → IO [FilePath]
getFilteredDirectoryContents fp = filter (`notElem` [".",".."]) <$> getDirectoryContents fp

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
  overlayName  <- readFile $ treePath </> "profiles/repo_name"
  treeCats     <- getFilteredDirectoryContents treePath
  filteredCats <- filterM (\(f, _) -> getFileStatus f <&> isDirectory)
                      $ map (\c -> (treePath </> c, c))
                            (filter (`notElem` [".git","eclass","metadata","profiles"]) treeCats)
  catMap <- mapM (\(fcat, cat) -> do
                    packages <- getFilteredDirectoryContents fcat
                    packagesFiltered <- filterM (\(fp, _) -> getFileStatus fp <&> isDirectory)
                                            $ map (\p -> (fcat </> p, p))
                                                  (filter (`notElem` ["metadata.xml"]) packages)
                    parsed <- mapM (\(fp, pn) -> do
                                versions <- getVersions fp pn overlayName
                                return $ Package cat versions pn
                              ) packagesFiltered
                    return (parsed, (cat, map snd packagesFiltered))
                 ) filteredCats -- [[package], a]
  let pkgs = concatMap fst catMap
      cpkg = map snd catMap
  return (pkgs, (overlayName, (treePath, cpkg)))

splitOnAnyOf ∷ Eq a ⇒ [[a]] → [a] → [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

parseOverlays ∷ String → IO (Tree, [OverlayMeta])
parseOverlays input = do
  let overlys = filter (not . null) $ splitOnAnyOf ["\\n","\\t"," ","'","$"] input
  parsed <- mapM parseOverlay overlys
  let treePkgs  = concatMap fst parsed
      trees     = map (\p -> (pCategory p ++ "/" ++ pName p, p)) treePkgs
      ovMetas   = map snd parsed
  return (M.fromList trees, ovMetas)

mergePackages ∷ Package → Package → Package
mergePackages p1 p2 =
  let versions  = S.union (pVersions p1) (pVersions p2)
  in Package (pCategory p1) versions (pName p1)

findExact ∷ [String] → Maybe String
findExact []  = Nothing
findExact [x] = Just x
findExact xss = Just (snd $ maximum $ [(length xs, xs) | xs <- xss])

getPackage ∷ String → String → Maybe String → String → IO (Maybe (Atom, Package))
getPackage _ _ Nothing _ = return Nothing
getPackage fcat cat (Just pn) vp = do
  overlay <- readFile $ fcat </> vp </> "repository"
  let versions = S.fromList [getVersionInstalled overlay pn vp]
  return $ Just (cat ++ "/" ++ pn, Package cat versions pn)

findPackages ∷ String → String → [String] → [String] → IO Tree
findPackages fcat cat versionedPkgs pkgs = do
  l <- mapM (\vpkg -> let mb = filter (`isPrefixOf` vpkg) pkgs
                      in getPackage fcat cat (findExact mb) vpkg
            ) versionedPkgs
  return $ M.fromList (catMaybes l)

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

portageConfig ∷ IO PortageConfig
portageConfig = do
  makeConf <- getConfigFile "/etc/portage/make.conf"
  let treePath = makeConf M.! "PORTDIR"

  (catMap, (ovName, (_, categories))) <- parseOverlay treePath

  (ov, met) <- case M.lookup "PORTDIR_OVERLAY" makeConf of
                  Just o  -> parseOverlays o
                  Nothing -> return (M.empty, [])

  let atoms       = map (\p -> (pCategory p ++ "/" ++ pName p, p)) catMap
      pkgs        = M.fromList atoms
      merged      = M.unionWith mergePackages pkgs ov
      metaList    = (ovName, (treePath, categories)) : met
      overlays    = M.fromList metaList

  -- TODO: learn haskell LOL
  -- https://stackoverflow.com/questions/8716728/resource-exhausted-too-many-open-files
  -- installed <- getInstalledPackages "/var/db/pkg" categories
  -- let finalTree = M.unionWith mergePackages merged installed

  return ( PortageConfig makeConf categories merged overlays )

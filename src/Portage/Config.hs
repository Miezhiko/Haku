{-# LANGUAGE UnicodeSyntax #-}
module Portage.Config where

import           Portage.Version
import           Portage.Package

import           Data.Functor
import           Data.List
import           Data.List.Split
import qualified Data.Map           as M
import           Data.Maybe

import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Control.Monad

type EnvMap = M.Map String String

-- Atom is category/PN
type Atom     = String
type Tree     = M.Map Atom Package
type Overlays = M.Map String (FilePath, [(String, [String])])
type OverlayMeta =  (String, (FilePath, [(String, [String])]))

data PortageConfig
  = PortageConfig
      { pcMakeConf   :: EnvMap
      , pcCategories :: [(String, [String])]
      , pcTree       :: Tree
      , pcInstalled  :: Tree
      , pcOverlays   :: Overlays
      }

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

getVersions ∷ String → String → String → IO [PackageVersion]
getVersions fp pn o = do
  dirContent <- getFilteredDirectoryContents fp
  let ebuilds   = filter (isSuffixOf ".ebuild") dirContent
      versions  = map (getVersion o pn) ebuilds
  return versions

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
  let versions = pVersions p1 ++ pVersions p2
  in Package (pCategory p1) versions (pName p1)

findExact :: [String] -> Maybe String
findExact []  = Nothing
findExact [x] = Just x
findExact xss = Just (snd $ maximum $ [(length xs, xs) | xs <- xss])

-- TODO: parse versions
findPackages :: String -> [String] -> [String] -> [(Atom, Package)]
findPackages cat versionedPkgs pkgs =
  let findMap = map (\vpkg ->
                      let mb = filter (\pkg -> isPrefixOf pkg vpkg) pkgs
                      in findExact mb
                    ) versionedPkgs
      pureMap = catMaybes findMap
  in map (\pn -> (cat ++ "/" ++ pn, Package cat [] pn)) pureMap

getInstalledPackages :: FilePath -> [(String, [String])] -> IO Tree
getInstalledPackages pkgdb categories = do
  treeCats      <- getFilteredDirectoryContents pkgdb
  filteredCats  <- filterM (\(f, _) -> getFileStatus f <&> isDirectory)
                      $ map (\c -> (pkgdb </> c, c))
                            treeCats
  catMap <- mapM (\(fcat, cat) -> do
                    packages <- getFilteredDirectoryContents fcat
                    let myCat = find (\(c, _) -> cat == c
                                     ) $ categories
                    case myCat of
                      Just (_,pkgs)  -> return $ findPackages cat packages pkgs
                      Nothing        -> return []
                 ) filteredCats
  let bigPackagesMap = concat catMap
  return $ M.fromList bigPackagesMap

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

  installed <- getInstalledPackages "/var/db/pkg" categories

  -- | TODO: maybe add new categories from overlays
  return ( PortageConfig makeConf categories merged installed overlays )

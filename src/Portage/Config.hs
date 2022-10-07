{-# LANGUAGE UnicodeSyntax #-}
module Portage.Config where

import           Portage.Version

import           Data.Functor
import           Data.List
import           Data.List.Split
import qualified Data.Map           as M

import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Control.Monad

type EnvMap = M.Map String String

data Package
  = Package
      { pCategory :: String
      , pVersions :: [PackageVersion]
      , pName     :: String
      }

instance Show Package where
  show (Package c _ n) = c ++ "/" ++ n

type Atom = String
type Tree = M.Map Atom Package

data PortageConfig
  = PortageConfig
      { pcMakeConf   :: EnvMap
      , pcCategories :: [String]
      , pcTree       :: Tree
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

parseOverlay ∷ FilePath → IO ([[Package]], [(String, String)])
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
                    mapM (\(fp, pn) -> do
                            versions <- getVersions fp pn overlayName
                            return $ Package cat versions pn
                         ) packagesFiltered
                 ) filteredCats
  return (catMap, filteredCats)

splitOnAnyOf ∷ Eq a ⇒ [[a]] → [a] → [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

parseOverlays ∷ String → IO Tree
parseOverlays input = do
  let overlys = filter (not . null) $ splitOnAnyOf ["\\n","\\t"," ","'","$"] input
  parsed <- mapM parseOverlay overlys
  let treePkgs = concat $ concatMap fst parsed
      trees = map (\p -> (pName p, p)) treePkgs
  return $ M.fromList trees

foo ∷ Package → Package → Package
foo p1 p2 =
  let versions = pVersions p1 ++ pVersions p2
  in Package (pCategory p1) versions (pName p1)

portageConfig ∷ IO PortageConfig
portageConfig = do
  makeConf <- getConfigFile "/etc/portage/make.conf"
  let treePath = makeConf M.! "PORTDIR"

  (catMap, filteredCats) <- parseOverlay treePath

  overlays <- case M.lookup "PORTDIR_OVERLAY" makeConf of
                  Just o  -> parseOverlays o
                  Nothing -> return M.empty

  let allPkgs     = concat catMap
      atoms       = map (\p -> (pName p, p)) allPkgs
      pkgs        = M.fromList atoms
      categories  = map snd filteredCats
      merged      = M.unionWith foo pkgs overlays

  return ( PortageConfig makeConf categories merged )

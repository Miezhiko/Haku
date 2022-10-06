{-# LANGUAGE UnicodeSyntax #-}
module Portage.Config where

import           Portage.Version

import           Data.Functor
import           Data.List
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
      , pVersions :: [Version]
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

getVersions ∷ String → String → IO [Version]
getVersions fp pn = do
  dirContent <- getFilteredDirectoryContents fp
  let ebuilds   = filter (isSuffixOf ".ebuild") dirContent
      versions  = map (getVersion pn) ebuilds
  return versions

portageConfig ∷ IO PortageConfig
portageConfig = do
  makeConf <- getConfigFile "/etc/portage/make.conf"
  let treePath = makeConf M.! "PORTDIR"

  treeCats     <- getFilteredDirectoryContents treePath
  filteredCats <- filterM (\(f, _) -> getFileStatus f <&> isDirectory)
                      $ map (\c -> (treePath </> c, c))
                            (filter (`notElem` [".git","eclass"]) treeCats)
  catMap <- mapM (\(fcat, cat) -> do
                    packages <- getFilteredDirectoryContents fcat
                    packagesFiltered <- filterM (\(fp, _) -> getFileStatus fp <&> isDirectory)
                                            $ map (\p -> (fcat </> p, p))
                                                  (filter (`notElem` ["metadata.xml"]) packages)
                    mapM (\(fp, pn) -> do
                            versions <- getVersions fp pn
                            return $ Package cat versions pn
                         ) packagesFiltered
                 ) filteredCats

  let allPkgs     = concat catMap
      atoms       = map (\p -> (pName p, p)) allPkgs
      pkgs        = M.fromList atoms
      categories  = map snd filteredCats

  return ( PortageConfig makeConf categories pkgs )

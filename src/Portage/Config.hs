{-# LANGUAGE UnicodeSyntax #-}
module Portage.Config where

import           Data.Functor
import qualified Data.Map           as M

import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Control.Monad

type EnvMap = M.Map String String

data Package
  = Package
      { category :: String
      , name     :: String
      }

instance Show Package where
  show (Package c n) = c ++ "/" ++ n

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

portageConfig ∷ IO PortageConfig
portageConfig = do
  makeConf <- getConfigFile "/etc/portage/make.conf"
  let treePath = makeConf M.! "PORTDIR"

  treeCats     <- getFilteredDirectoryContents treePath
  filteredCats <- filterM (\(f, _) -> getFileStatus f <&> isDirectory)
                      $ map (\p -> (treePath </> p, p))
                            (filter (`notElem` [".git","eclass"]) treeCats)
  catMap <- mapM (\(fcat, cat) -> do
                    packages <- getFilteredDirectoryContents fcat
                    return $ map (Package cat) packages
                 ) filteredCats

  let allPkgs     = concat catMap
      atoms       = map (\p -> (name p, p)) allPkgs
      pkgs        = M.fromList atoms
      categories  = map snd filteredCats

  return ( PortageConfig makeConf categories pkgs )

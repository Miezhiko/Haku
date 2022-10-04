{-# LANGUAGE UnicodeSyntax #-}
module Portage.Config where

import qualified Data.Map         as M

import           System.Directory
import           System.Process

type EnvMap = M.Map String String

data PortageConfig
  = PortageConfig
      { makeConf :: EnvMap
      , tree     :: [FilePath]
      }

parseEnvMap ∷ String → EnvMap
parseEnvMap s = M.fromList $
                   [  (v,stripQuotes c) | 
                      l <- lines s,
                      (v,'=':c) <- return $ break (=='=') l ]
  where  stripQuotes ('\'':r@(_:_)) =  init r
         stripQuotes x              =  x

getConfigFile ∷ FilePath → IO EnvMap
getConfigFile f =  do  (_,r,_) <- readCreateProcessWithExitCode (
                                    shell $  "unset $(set | sed 's/^\\([^=]*\\)=.*$/\\1/') 2>/dev/null;" ++
                                             "source " ++ f ++ "; set" ) []
                       return (parseEnvMap r)

portageConfig ∷ IO PortageConfig
portageConfig = do
  makeConf <- getConfigFile "/etc/portage/make.conf"
  let treePath = makeConf M.! "PORTDIR"
  tree     <- getDirectoryContents treePath
  let filtered = filter (`notElem` [".","..",".git","eclass"]) tree
  return ( PortageConfig makeConf filtered )

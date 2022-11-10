{-# LANGUAGE
    UnicodeSyntax
  #-}

module Paths where

import           System.Directory
import           System.FilePath  ((</>))

getHakuCachePath ∷ IO FilePath
getHakuCachePath = (</> "haku.cache") <$> getHomeDirectory

getShelterConfPath ∷ IO FilePath
getShelterConfPath = (</> ".shelter.yml") <$> getHomeDirectory

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module Portage.Version
  ( module Portage.Types.Version
  , getVersion
  , getVersionInstalled
  ) where

import           Portage.Types.Version

getVersion ∷ String → String → String → PackageVersion
getVersion overlay pn ebuild = do
  let noebuild  = take (length ebuild - 7) ebuild
      ver       = drop (length pn + 1) noebuild
      version   = case parseVersion ver of
                    Left   _ -> error $ "getVersion: version parse error '" ++ ver ++ "'"
                    Right  x ->  x
  PackageVersion version overlay False

getVersionInstalled ∷ String → String → String → PackageVersion
getVersionInstalled overlay pn path = do
  let ver       = drop (length pn + 1) path
      version   = case parseVersion ver of
                    Left   _ -> error $ "getVersion: version parse error '" ++ ver ++ "'"
                    Right  x ->  x
  PackageVersion version overlay True

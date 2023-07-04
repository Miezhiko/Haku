module Portage.Version
  ( module Portage.Types.Version
  , getVersion
  , getVersionInstalled
  , isLive
  , isLiveVersion
  ) where

import           Prelude.Unicode

import           Portage.Types.Version

getVersion ∷ String -> String -> String -> PackageVersion
getVersion overlay pn ebuild = do
  let noebuild  = take (length ebuild - 7) ebuild
      ver       = drop (length pn + 1) noebuild
      version   = case parseVersion ver of
                    Left   _ -> error $ "getVersion: version parse error '" ++ ver ++ "'"
                    Right  x ->  x
  PackageVersion version overlay False

getVersionInstalled ∷ String -> String -> String -> Either String PackageVersion
getVersionInstalled overlay pn path =
  let ver = drop (length pn + 1) path
  in case parseVersion ver of
      Left   _ -> Left  $ "getVersionInstalled: version parse error '" ++ ver ++ "'"
      Right  x -> Right $ PackageVersion x overlay True

isLiveVersion ∷ Version -> Bool
isLiveVersion (Version [] _ _ _ _)    = False
isLiveVersion (Version [ver] _ _ _ _) = ver ∈ [9999, 99999999]
isLiveVersion (Version xs _ _ _ _)    = any (∈ [9999, 99999999]) xs

isLive ∷ PackageVersion -> Bool
isLive = isLiveVersion ∘ pvVersion

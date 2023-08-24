module Portage.Version
  ( module Portage.Types.Version
  , getVersion
  , getVersionInstalled
  , isLive
  , isBeta
  , isLiveVersion
  , isBetaVersion
  ) where

import           Prelude.Unicode

import           Portage.Types.Version

getVersion ∷ String -> String -> String -> PackageVersion
getVersion overlay pn ebuild = 
  let noebuild = take (length ebuild - 7) ebuild
      ver      = drop (length pn + 1) noebuild
  in case parseVersion ver of
      Left _        -> error $ "getVersion: version parse error '" ++ ver ++ "'"
      Right version -> PackageVersion version overlay False

getVersionInstalled ∷ String -> String -> String -> Either String PackageVersion
getVersionInstalled overlay pn path =
  case parseVersion ver of
    Left   _ -> Left  $ "getVersionInstalled: version parse error '" ++ ver ++ "'"
    Right  x -> Right $ PackageVersion x overlay True
 where ver = drop (length pn + 1) path

isLiveVersion ∷ Version -> Bool
isLiveVersion (Version [] _ _ _ _)    = False
isLiveVersion (Version [ver] _ _ _ _) = ver ∈ [9999, 99999999]
isLiveVersion (Version xs _ _ _ _)    = any (∈ [9999, 99999999]) xs

isLive ∷ PackageVersion -> Bool
isLive = isLiveVersion ∘ pvVersion

isAlphaOrBeta :: Suffix -> Bool
isAlphaOrBeta (Alpha _) = True
isAlphaOrBeta (Beta _)  = True
isAlphaOrBeta _         = False

isBetaVersion ∷ Version -> Bool
isBetaVersion (Version _ _ [] _ _)    = False
isBetaVersion (Version _ _ [x] _ _)   = isAlphaOrBeta x
isBetaVersion (Version _ _ xs _ _)    = any isAlphaOrBeta xs

isBeta ∷ PackageVersion -> Bool
isBeta = isBetaVersion ∘ pvVersion

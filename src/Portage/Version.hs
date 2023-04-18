module Portage.Version
  ( module Portage.Types.Version
  , getVersion
  , getVersionInstalled
  , isLive
  ) where

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
isLiveVersion (Version ver _ _ _ _) = 9999 `elem` ver

isLive ∷ PackageVersion -> Bool
isLive pv = isLiveVersion (pvVersion pv)

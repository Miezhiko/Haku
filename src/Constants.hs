{-# LANGUAGE
    Safe
  , UnicodeSyntax
  #-}
module Constants where

constMakeConfPath ∷ String
constMakeConfPath = "/etc/portage/make.conf"

constInstalledPath ∷ String
constInstalledPath = "/var/db/pkg"

cosntSudoPath ∷ String
cosntSudoPath = "/usr/bin/sudo"

constProfilesPackageMask ∷ String
constProfilesPackageMask = "profiles/package.mask"

constPortagePackageMask ∷ String
constPortagePackageMask = "/etc/portage/package.mask"

constProfilesRepoName ∷ String
constProfilesRepoName = "profiles/repo_name"

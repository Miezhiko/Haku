{-# LANGUAGE
    Safe
  , UnicodeSyntax
  #-}
module Constants where

constMakeConfPath         ∷ String
constInstalledPath        ∷ String
constWorldFile            ∷ String
cosntSudoPath             ∷ String
constProfilesPackageMask  ∷ String
constPortagePackageMask   ∷ String
constProfilesRepoName     ∷ String

constMakeConfPath         = "/etc/portage/make.conf"
constInstalledPath        = "/var/db/pkg"
constWorldFile            = "/var/lib/portage/world"
cosntSudoPath             = "/usr/bin/sudo"
constProfilesPackageMask  = "profiles/package.mask"
constPortagePackageMask   = "/etc/portage/package.mask"
constProfilesRepoName     = "profiles/repo_name"

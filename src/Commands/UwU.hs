{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Commands.UwU where

import           Constants         (cosntSudoPath)
import           Types
import           Utils

import           Portage.Config    (loadPortageConfig)

import           Shelter.Checker

import           System.Directory  (doesFileExist)
import           System.Posix.User (getRealUserID)

{- HLINT ignore "Redundant <$>" -}
runUpgradeScripts ∷ IO ()
runUpgradeScripts = (== 0) <$> getRealUserID >>= \root →
  if root then do
    doesFileExist "/usr/bin/shelter" >>= \shelterBinExists ->
      if shelterBinExists
        then rawAndIgnore "shelter" 𝜀
        else updateAll
    rawAndIgnore "egencache" ["--repo=gentoo", "--update"]
    rawAndIgnore "eix-update" 𝜀
    rawAndIgnore "emerge" [ "-avuDN", "@world"
                          , "--backtrack=100"
                          , "--with-bdeps=y"
                          , "--quiet-build=n"
                          ]
  else doesFileExist cosntSudoPath >>= \sudoExists →
    if sudoExists
      then do
        putStrLn "running with sudo (not recommended)"
        rawAndIgnore "sudo" ["shelter"]
        rawAndIgnore "sudo" ["egencache", "--repo=gentoo", "--update"]
        rawAndIgnore "sudo" ["eix-update"]
        rawAndIgnore "sudo" [ "emerge", "-avuDN", "@world"
                            , "--backtrack=100"
                            , "--with-bdeps=y"
                            , "--quiet-build=n"
                            ]
      else putStrLn "should run as root or have sudo installed"

owo ∷ HakuMonad m ⇒ String → [String] → m ()
owo _ _ = ask >>= \env → do
  liftIO $ do runUpgradeScripts
              pc <- loadPortageConfig
              writeIORef (config env) pc { pcUpdateCache = True }

uwuCmd ∷ Command String m
uwuCmd = Command { command = ["uwu"]
                 , description = "Update and upgrade the world (alias)"
                 , usage = ("haku " ++)
                 , state = 𝜀
                 , options = const 𝜀
                 , handler = owo }

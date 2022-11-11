{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Commands.UwU where

import           Constants         (cosntSudoPath)
import           Types
import           Utils

import           Portage.Config    (portageConfig)

import           System.Directory  (doesFileExist)
import           System.Posix.User (getRealUserID)

{- HLINT ignore "Redundant <$>" -}
uwu ∷ IORef PortageConfig → String → [String] → IO ()
uwu _ _ _ = (== 0) <$> getRealUserID >>= \root ->
  if root then do
    rawAndIgnore "shelter" 𝜀
    rawAndIgnore "egencache" ["--repo=gentoo", "--update"]
    rawAndIgnore "eix-update" 𝜀
    rawAndIgnore "emerge" [ "-avuDN", "@world"
                          , "--backtrack=100"
                          , "--with-bdeps=y"
                          , "--quiet-build=n"
                          ]
  else doesFileExist cosntSudoPath >>= \sudoExists ->
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
owo c xs = ask >>= \env -> do
  liftIO $ uwu (config env) c xs
  void $ liftIO ( portageConfig (handle env) )

uwuCmd ∷ Command String m
uwuCmd = Command { command = ["uwu"]
                 , description = "Update and upgrade the world (alias)"
                 , usage = ("haku " ++)
                 , state = 𝜀
                 , options = const 𝜀
                 , handler = owo }

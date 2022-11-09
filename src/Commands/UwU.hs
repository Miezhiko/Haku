{-# LANGUAGE UnicodeSyntax #-}
module Commands.UwU where

import           Constants         (cosntSudoPath)
import           Types
import           Utils

import           System.Directory  (doesFileExist)
import           System.Posix.User (getRealUserID)

{- HLINT ignore "Redundant <$>" -}
uwu ∷ IORef PortageConfig → String → [String] → IO ()
uwu _ _ _ = (== 0) <$> getRealUserID >>= \root ->
  if root then do
    rawAndIgnore "shelter" 𝜀
    -- TODO: this is broken because file is busy
    -- maybe should use context with file handle
    -- and put PortageConfig inside context
    --portageConfig >>= \newConfig -> do
      -- writeIORef rpc newConfig
      --storeConfig newConfig
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

uwuCmd ∷ Command String
uwuCmd = Command { command = ["uwu"]
                 , description = "Update and upgrade the world (alias)"
                 , usage = ("haku " ++)
                 , state = 𝜀
                 , options = const 𝜀
                 , handler = uwu }

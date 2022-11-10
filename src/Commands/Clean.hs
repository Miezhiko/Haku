{-# LANGUAGE UnicodeSyntax #-}
module Commands.Clean where

import           Constants         (cosntSudoPath)
import           Types
import           Utils

import           System.Directory  (doesFileExist)
import           System.Posix.User (getRealUserID)

{- HLINT ignore "Redundant <$>" -}
clean ∷ IORef PortageConfig → String → [String] → IO ()
clean _ _ _ = (== 0) <$> getRealUserID >>= \r ->
  if r then rawAndIgnore "emerge" [ "--depclean" ]
       else doesFileExist cosntSudoPath >>= \sudoExists ->
              if sudoExists then rawAndIgnore "sudo" [ "emerge", "--depclean" ]
                            else putStrLn "should run as root or have sudo installed"

cleanCmd ∷ Command String m
cleanCmd = Command { command = ["clean"]
                   , description = "Clean world"
                   , usage = ("haku " ++)
                   , state = 𝜀
                   , options = const 𝜀
                   , handler = liftMyAss clean }

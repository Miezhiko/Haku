{-# LANGUAGE
    UnicodeSyntax
  #-}

module Commands.Clean where

import           Constants         (cosntSudoPath)
import           Types
import           Utils

import           System.Directory  (doesFileExist)
import           System.Posix.User (getRealUserID)

{- HLINT ignore "Redundant <$>" -}
clean ∷ IO ()
clean = (== 0) <$> getRealUserID >>= \r →
  if r then rawAndIgnore "emerge" [ "--depclean" ]
       else doesFileExist cosntSudoPath >>= \sudoExists →
              if sudoExists then messageRunningWithSudo
                              >> rawAndIgnore "sudo" [ "emerge", "--depclean" ]
                            else messageShouldRunAsRoot

cleanCmd ∷ Command String m
cleanCmd = Command { command = ["clean"]
                   , description = "Clean world"
                   , usage = ("haku " ++)
                   , state = 𝜀
                   , options = const 𝜀
                   , handler = \_ _ → liftIO clean }

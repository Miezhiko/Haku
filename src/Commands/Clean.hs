{-# LANGUAGE
    UnicodeSyntax
  #-}

module Commands.Clean where

import           Types
import           Utils

clean ∷ IO ()
clean = isRoot ( rawAndIgnore "emerge" [ "--depclean" ] )
               ( rawAndIgnore "sudo" [ "emerge", "--depclean" ] )

cleanCmd ∷ Command String m
cleanCmd = Command { command = ["clean"]
                   , description = "Clean world"
                   , usage = ("haku " ++)
                   , state = 𝜀
                   , options = const 𝜀
                   , handler = \_ _ → liftIO clean }

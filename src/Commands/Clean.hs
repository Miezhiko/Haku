{-# LANGUAGE UnicodeSyntax #-}
module Commands.Clean where

import           Types
import           Utils

clean ∷ IORef PortageConfig → String → [String] → IO ()
clean _ _ _ = rawAndIgnore "emerge" [ "--depclean" ]

cleanCmd ∷ Command String
cleanCmd = Command
              {
                command = ["clean"],
                description = "Clean world",
                usage = ("haku " ++),
                state = [],
                options = const [],
                handler = clean
              }

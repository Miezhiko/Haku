{-# LANGUAGE UnicodeSyntax #-}
module Commands.Update where

import           Types
import           Utils

update ∷ IO ()
update = do
  rawAndIgnore "shelter" []
  rawAndIgnore "emerge" ["--sync"]
  rawAndIgnore "egencache" ["--repo=gentoo", "--update"]
  rawAndIgnore "eix-update" []

updateCmd ∷ Command String
updateCmd = Command
              {
                command = ["update"],
                description = "Update world",
                usage = ("haku " ++),
                state = [],
                options = const [],
                handler = \_ _ _ -> update
              }

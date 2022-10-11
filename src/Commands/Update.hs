{-# LANGUAGE UnicodeSyntax #-}
module Commands.Update where

import           Types
import           Utils

update ∷ IO ()
update = do
  runIfExists "/usr/bin/shelter"    "shelter" []
  runIfExists "/usr/bin/emerge"     "emerge" ["--sync"]
  runIfExists "/usr/bin/egencache"  "egencache" ["--repo=gentoo", "--update"]
  runIfExists "/usr/bin/eix-update" "eix-update" []

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

{-# LANGUAGE UnicodeSyntax #-}
module Commands.Upgrade where

import           Types
import           Utils

upgrade ∷ IO ()
upgrade = do
  rawAndIgnore "emerge" [ "-avuDN"
                        , "@world"
                        , "--backtrack=100"
                        , "--with-bdeps=y"
                        , "--quiet-build=n"
                        ]

upgradeCmd ∷ Command String
upgradeCmd = Command
              {
                command = ["upgrade"],
                description = "Upgrade world",
                usage = ("haku " ++),
                state = [],
                options = const [],
                handler = \_ _ _ -> upgrade
              }

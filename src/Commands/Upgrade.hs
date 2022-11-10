{-# LANGUAGE UnicodeSyntax #-}
module Commands.Upgrade where

import           Types
import           Utils

upgrade ∷ IO ()
upgrade =
  rawAndIgnore "emerge" [ "-avuDN"
                        , "@world"
                        , "--backtrack=100"
                        , "--with-bdeps=y"
                        , "--quiet-build=n"
                        ]

upgradeCmd ∷ Command String m
upgradeCmd = Command { command = ["upgrade"]
                     , description = "Upgrade world"
                     , usage = ("haku " ++)
                     , state = 𝜀
                     , options = const 𝜀
                     , handler = \_ _ _ -> liftIO upgrade }

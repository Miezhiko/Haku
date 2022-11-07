{-# LANGUAGE UnicodeSyntax #-}
module Commands.UwU where

import           Types
import           Utils

uwu ∷ IORef PortageConfig → String → [String] → IO ()
uwu _ _ _ = do
  rawAndIgnore "shelter" 𝜀
  rawAndIgnore "egencache" ["--repo=gentoo", "--update"]
  rawAndIgnore "eix-update" 𝜀
  rawAndIgnore "emerge" [ "-avuDN"
                        , "@world"
                        , "--backtrack=100"
                        , "--with-bdeps=y"
                        , "--quiet-build=n"
                        ]

uwuCmd ∷ Command String
uwuCmd = Command
              {
                command = ["uwu"],
                description = "Update and upgrade the world (alias)",
                usage = ("haku " ++),
                state = 𝜀,
                options = const 𝜀,
                handler = uwu
              }

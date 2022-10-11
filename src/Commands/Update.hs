{-# LANGUAGE UnicodeSyntax #-}
module Commands.Update where

import           Types
import           Utils

import           Control.Monad

data UpdateState
  = UpdateState
      { updUpgrade :: Bool
      , updMinimal :: Bool
      }

updateOpts ∷ Bool → [OptDescr (UpdateState → UpdateState)]
updateOpts _ =
    [ Option "u" ["upgrade"] (NoArg (\s -> s { updUpgrade = True })) "run upgrade after"
    ]

update ∷ UpdateState → IO ()
update upds = do
  runIfExists "/usr/bin/shelter"    "shelter" []
  runIfExists "/usr/bin/emerge"     "emerge" ["--sync"]
  runIfExists "/usr/bin/egencache"  "egencache" ["--repo=gentoo", "--update"]
  runIfExists "/usr/bin/eix-update" "eix-update" []
  when (updUpgrade upds) $
    rawAndIgnore "emerge" [ "-avuDN"
                          , "@world"
                          , "--backtrack=100"
                          , "--with-bdeps=y"
                          , "--quiet-build=n"
                          ]

updateCmd ∷ Command UpdateState
updateCmd = Command
              {
                command = ["update"],
                description = "Update world",
                usage = ("haku " ++),
                state = UpdateState { updUpgrade = False
                                    , updMinimal = False },
                options = updateOpts,
                handler = \_ upds _ -> update upds
              }

{-# LANGUAGE UnicodeSyntax #-}
module Commands.Update where

import           Types
import           Utils

import           Control.Monad

data UpdateState
  = UpdateState
      { updUpgrade :: Bool
      , updMinimal :: Bool
      , updStore   :: Bool
      }

updateOpts ∷ Bool → [OptDescr (UpdateState → UpdateState)]
updateOpts _ =
    [ Option "u" ["upgrade"] (NoArg (\s -> s { updUpgrade = True })) "run upgrade after"
    , Option "m" ["minimal"] (NoArg (\s -> s { updMinimal = True })) "only emerge --sync"
    , Option "s" ["store"]   (NoArg (\s -> s { updStore = True }))   "store new config after update"
    ]

update ∷ IORef PortageConfig → UpdateState → [String] → IO ()
update _ upds _ = do
  let minimal = updMinimal upds
  unless minimal $ runIfExists "/usr/bin/shelter" "shelter" []
  runIfExists "/usr/bin/emerge" "emerge" ["--sync"]
  unless minimal $ runIfExists "/usr/bin/egencache" "egencache" ["--repo=gentoo", "--update"]
  unless minimal $ runIfExists "/usr/bin/eix-update" "eix-update" []
  when (updStore upds) $
    portageConfig >>= storeConfig
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
                command = ["u", "update"],
                description = "Update world",
                usage = ("haku " ++),
                state = UpdateState { updUpgrade  = False
                                    , updMinimal  = False
                                    , updStore    = False },
                options = updateOpts,
                handler = update
              }

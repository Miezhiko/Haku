{-# LANGUAGE
    UnicodeSyntax
  #-}

module Commands.Upgrade where

import           Types
import           Utils

data UpgradeState
  = UpgradeState
      { upgrdSelf    :: Bool
      , upgrdVerbose :: Bool
      }

upgradeOpts ∷ Bool → [OptDescr (UpgradeState → UpgradeState)]
upgradeOpts _ =
    [ Option "s" ["self"]     (NoArg (\s → s { upgrdSelf = True })) "upgrade self"
    , Option "v" ["verbose"]  (NoArg (\s → s { upgrdVerbose = True })) "more things..."
    ]

upgrade ∷ UpgradeState → IO ()
upgrade ugrs = if upgrdSelf ugrs
  then rawAndIgnore "emerge" [ "haku" ]
  else rawAndIgnore "emerge" [ "-avuDN"
                             , "@world"
                             , "--backtrack=100"
                             , "--with-bdeps=y"
                             , "--quiet-build=n"
                             ]

upgradeCmd ∷ Command UpgradeState m
upgradeCmd = Command { command = ["upgrade"]
                     , description = "Upgrade world"
                     , usage = ("haku " ++)
                     , state = UpgradeState { upgrdSelf     = False
                                            , upgrdVerbose  = False }
                     , options = upgradeOpts
                     , handler = \ugrs _ → liftIO $ upgrade ugrs }

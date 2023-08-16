module Commands.Upgrade
  ( upgradeCmd
  ) where

import           Types
import           Utils

data UpgradeState
  = UpgradeState
      { upgrdSelf    :: Bool
      , upgrdVerbose :: Bool
      }

upgradeOpts ∷ Bool -> [OptDescr (UpgradeState -> UpgradeState)]
upgradeOpts _ =
  [ Option "s" ["self"]     (NoArg (\s -> s { upgrdSelf = True })) "upgrade self"
  , Option "v" ["verbose"]  (NoArg (\s -> s { upgrdVerbose = True })) "more things..."
  ]

upgradeRoot ∷ UpgradeState -> IO ()
upgradeRoot ugrs = if upgrdSelf ugrs
  then rawAndIgnore "emerge" [ "haku" ]
  else rawAndIgnore "emerge" [ "-avuDN"
                             , "@world"
                             , "--backtrack=100"
                             , "--with-bdeps=y"
                             , "--quiet-build=n"
                             ]

upgradeSudo ∷ UpgradeState -> IO ()
upgradeSudo ugrs = if upgrdSelf ugrs
  then rawAndIgnore "sudo" [ "emerge", "haku" ]
  else rawAndIgnore "sudo" [ "emerge"
                           , "-avuDN"
                           , "@world"
                           , "--backtrack=100"
                           , "--with-bdeps=y"
                           , "--quiet-build=n"
                           ]

upgrade ∷ UpgradeState -> IO ()
upgrade = liftM2 isRoot upgradeRoot
                        upgradeSudo

upgradeCmd ∷ Command UpgradeState m
upgradeCmd = Command { command      = ["upgrade"]
                     , description  = "Upgrade world"
                     , usage        = ("haku " ++)
                     , state        = UpgradeState { upgrdSelf     = False
                                                   , upgrdVerbose  = False }
                     , options      = upgradeOpts
                     , handler      = \ugrs _ -> liftIO $ upgrade ugrs }

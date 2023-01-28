{-# LANGUAGE
    UnicodeSyntax
  #-}

module Commands.Upgrade where

import           Constants         (cosntSudoPath)
import           Types
import           Utils

import           System.Directory  (doesFileExist)
import           System.Posix.User (getRealUserID)

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

{- HLINT ignore "Redundant <$>" -}
upgrade ∷ UpgradeState → IO ()
upgrade ugrs = (== 0) <$> getRealUserID >>= \root → if root
  then if upgrdSelf ugrs
          then rawAndIgnore "emerge" [ "haku" ]
          else rawAndIgnore "emerge" [ "-avuDN"
                                     , "@world"
                                     , "--backtrack=100"
                                     , "--with-bdeps=y"
                                     , "--quiet-build=n"
                                     ]
  else doesFileExist cosntSudoPath >>= \sudoExists →
    if sudoExists
      then do
        messageRunningWithSudo
        if upgrdSelf ugrs
          then rawAndIgnore "sudo" [ "emerge", "haku" ]
          else rawAndIgnore "sudo" [ "emerge"
                                   , "-avuDN"
                                   , "@world"
                                   , "--backtrack=100"
                                   , "--with-bdeps=y"
                                   , "--quiet-build=n"
                                   ]
    else messageShouldRunAsRoot

upgradeCmd ∷ Command UpgradeState m
upgradeCmd = Command { command = ["upgrade"]
                     , description = "Upgrade world"
                     , usage = ("haku " ++)
                     , state = UpgradeState { upgrdSelf     = False
                                            , upgrdVerbose  = False }
                     , options = upgradeOpts
                     , handler = \ugrs _ → liftIO $ upgrade ugrs }

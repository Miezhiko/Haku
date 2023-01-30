{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Commands.Update where

import           Constants         (cosntSudoPath)
import           Types
import           Utils

import           Portage.Config (loadPortageConfig)

import           System.Directory  (doesFileExist)
import           System.Posix.User (getRealUserID)

data UpdateState
  = UpdateState
      { updUpgrade :: Bool
      , updMinimal :: Bool
      , updStore   :: Bool
      }

updateOpts ∷ Bool → [OptDescr (UpdateState → UpdateState)]
updateOpts _ =
    [ Option "u" ["upgrade"] (NoArg (\s → s { updUpgrade = True })) "run upgrade after"
    , Option "m" ["minimal"] (NoArg (\s → s { updMinimal = True })) "only emerge --sync"
    , Option "s" ["store"]   (NoArg (\s → s { updStore = True }))   "store new config after update"
    ]

{- HLINT ignore "Redundant <$>" -}
update ∷ IORef PortageConfig → UpdateState → IO ()
update rpc upds = (== 0) <$> getRealUserID >>= \root → if root
  then do
    rawAndIgnore "emerge" [ "--sync" ]
    unless (updMinimal upds) $ do
      runIfExists "/usr/bin/egencache" "egencache" [ "--repo=gentoo", "--update" ]
      runIfExists "/usr/bin/eix-update" "eix-update" 𝜀
    when (updStore upds) $ do
      pc ← loadPortageConfig
      writeIORef rpc pc { pcUpdateCache = True }
    when (updUpgrade upds) $
      rawAndIgnore "emerge" [ "-avuDN"
                            , "@world"
                            , "--backtrack=100"
                            , "--with-bdeps=y"
                            , "--quiet-build=n"
                            ]
  else doesFileExist cosntSudoPath >>= \sudoExists →
    if sudoExists
      then do
        rawAndIgnore "sudi" [ "emerge", "--sync" ]
        unless (updMinimal upds) $ do
          runIfExists "/usr/bin/egencache" "sudo" [ "egencache", "--repo=gentoo", "--update" ]
          runIfExists "/usr/bin/eix-update" "sudo" [ "eix-update" ]
        when (updStore upds) $ do
          pc ← loadPortageConfig
          writeIORef rpc pc { pcUpdateCache = True }
        when (updUpgrade upds) $
          rawAndIgnore "sudo" [ "emerge"
                              , "-avuDN"
                              , "@world"
                              , "--backtrack=100"
                              , "--with-bdeps=y"
                              , "--quiet-build=n"
                              ]
      else putStrLn "should run as root or have sudo installed"

updateMyAss ∷ HakuMonad m ⇒ UpdateState → [String] → m ()
updateMyAss us _ = ask >>= \env →
   liftIO $ update (config env) us

updateCmd ∷ Command UpdateState m
updateCmd = Command
            { command = ["u", "update"]
            , description = "Update world"
            , usage = ("haku " ++)
            , state = UpdateState { updUpgrade  = False
                                  , updMinimal  = False
                                  , updStore    = False }
            , options = updateOpts
            , handler = updateMyAss }

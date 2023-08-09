module Commands.UwU
  ( uwuCmd
  ) where

import           Types
import           Utils

import           System.Directory (doesFileExist)

import           Portage.Config   (loadPortageConfig)

import           Shelter.Checker

data UwuState
  = UwuState
      { uwuHaskellSync :: Bool
      , uwuAsk         :: Bool
      }

uwuOpts ∷ Bool -> [OptDescr (UwuState -> UwuState)]
uwuOpts _ =
  [ Option "hs" ["haskell-sync"] (NoArg (\s -> s { uwuHaskellSync = True })) "use haskell code to sync"
  , Option "a"  ["ask"]          (NoArg (\s -> s { uwuAsk = True })) "ask before upgrade"
  ]

runUpgradeScriptsRoot ∷ UwuState -> IO ()
runUpgradeScriptsRoot uws = do
  if uwuHaskellSync uws
    then updateAll
    else doesFileExist "/usr/bin/shelter" >>= \shelterBinExists ->
          if shelterBinExists
            then rawAndIgnore "shelter" 𝜀
            else updateAll
  putStrLn "regenerating Gentoo cache..."
  rawAndIgnore "egencache" ["--repo=gentoo", "--update"]
  rawAndIgnore "eix-update" 𝜀
  rawAndIgnore "emerge" $ [ "-avuDN", "@world"
                          , "--backtrack=100"
                          , "--with-bdeps=y"
                          , "--quiet-build=n"
                          ] ++ ["-a" | uwuAsk uws]

runUpgradeScriptsSudo ∷ UwuState -> IO ()
runUpgradeScriptsSudo uws = do
  when (uwuHaskellSync uws) $ putStrLn "WARNING: can't run sync with own code in sudo mode"
  runIfExists "/usr/bin/shelter" "sudo" ["shelter"]
  putStrLn "regenerating Gentoo cache..."
  rawAndIgnore "sudo" ["egencache", "--repo=gentoo", "--update"]
  rawAndIgnore "sudo" ["eix-update"]
  rawAndIgnore "sudo" $ [ "emerge", "-avuDN", "@world"
                        , "--backtrack=100"
                        , "--with-bdeps=y"
                        , "--quiet-build=n"
                        ] ++ ["-a" | uwuAsk uws]

runUpgradeScripts ∷ UwuState -> IO ()
runUpgradeScripts = liftM2 isRoot runUpgradeScriptsRoot
                                  runUpgradeScriptsSudo

owo ∷ HakuMonad m ⇒ UwuState -> [String] -> m ()
owo uws _ = ask >>= \env -> do
  liftIO $ do runUpgradeScripts uws
              pc <- loadPortageConfig
              writeIORef (config env) pc { pcUpdateCache = True }

uwuCmd ∷ Command UwuState m
uwuCmd = Command { command      = ["uwu"]
                 , description  = "Update and upgrade the world (alias)"
                 , usage        = ("haku " ++)
                 , state        = UwuState { uwuHaskellSync = False
                                           , uwuAsk = False }
                 , options      = uwuOpts
                 , handler      = owo }

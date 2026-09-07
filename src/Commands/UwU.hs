module Commands.UwU
  ( uwuCmd
  ) where

import           Types
import           Utils

import           Data.List         (isInfixOf, isSuffixOf)

import           System.Directory  (doesFileExist)
import           System.FilePath   ((</>))

import           Portage.Config    (loadPortageConfig, resolveMainRepoPath)

import           Shelter.Checker

data UwuState
  = UwuState
      { uwuHaskellSync :: Bool
      , uwuAsk         :: Bool
      , uwuPretend     :: Bool
      }

uwuOpts ∷ Bool -> [OptDescr (UwuState -> UwuState)]
uwuOpts _ =
  [ Option "hs" ["haskell-sync"] (NoArg (\s -> s { uwuHaskellSync = True }))  "use haskell code to sync"
  , Option "a"  ["ask"]          (NoArg (\s -> s { uwuAsk = True }))          "ask before upgrade"
  , Option "p"  ["pretend"]      (NoArg (\s -> s { uwuPretend = True }))      "just show planned updates"
  ]

-- | Is a git remote URL the canonical github.com/gentoo/gentoo mirror,
--   or a fork of it (same repo name, any owner)?
isGentooGithubRemote ∷ String -> Bool
isGentooGithubRemote url =
  "github.com" `isInfixOf` url ∧
    ("/gentoo" `isSuffixOf` stripDotGit (rstrip url))
 where stripDotGit u = if ".git" `isSuffixOf` u
                          then take (length u - 4) u
                          else u

-- | Whether the main tree is a git checkout tracking gentoo/gentoo
--   (or a fork of it) on GitHub -- if so `egencache`/`eix-update` make
--   sense, otherwise the tree isn't a rsync/webrsync/git-of-gentoo
--   checkout Haku knows how to refresh that way, so fall back to
--   a plain `emerge --sync`.
hasGentooGithubRemote ∷ FilePath -> IO Bool
hasGentooGithubRemote repoPath =
  readQuiet "git" ["--git-dir=" ++ repoPath </> ".git", "remote", "-v"] >>=
    \case Nothing  -> pure False
          Just out -> pure $ any remoteIsGentoo (lines out)
 where remoteIsGentoo l = case words l of
                            (_:url:_) -> isGentooGithubRemote url
                            _         -> False

syncTreeCommands ∷ String -> Bool -> [(String, [String])]
syncTreeCommands jobs True  = [ ("egencache", ["--repo=gentoo", "--update", jobs])
                               , ("eix-update", 𝜀) ]
syncTreeCommands _    False = [ ("emerge", ["--sync"]) ]

runUpgradeScriptsRoot ∷ UwuState
                     -> Bool -- main tree is a github.com/gentoo/gentoo (or fork) checkout
                     -> (String -> IO ())
                     -> IO ()
runUpgradeScriptsRoot uws isGentooGitCheckout hlog = do
  if uwuHaskellSync uws
    then updateAll
    else doesFileExist "/usr/bin/shelter" >>= \shelterBinExists ->
          if shelterBinExists
            then rawAndIgnore "shelter" 𝜀
            else updateAll
  runIfExists "/usr/bin/snap" "snap" ["refresh"]
  hlog "<Green>syncing Gentoo tree..."
  jobs <- jobsArg
  forM_ (syncTreeCommands jobs isGentooGitCheckout) (uncurry rawAndIgnore)
  rawAndIgnore "emerge" $ [ "-uDN", "@world"
                          , "--backtrack=100"
                          , "--with-bdeps=y"
                          , "--quiet-build=n"
                          , "--exclude=nvidia-drivers"
                          ] ++ ["-av" | uwuAsk uws]
                            ++ ["-pv" | uwuPretend uws]

runUpgradeScriptsSudo ∷ UwuState
                    -> Bool -- main tree is a github.com/gentoo/gentoo (or fork) checkout
                    -> (String -> IO ())
                    -> IO ()
runUpgradeScriptsSudo uws isGentooGitCheckout hlog = do
  when (uwuHaskellSync uws) $ hlog "<Red>WARNING: can't run sync with own code in sudo mode"
  runIfExists "/usr/bin/shelter" "sudo" ["shelter"]
  runIfExists "/usr/bin/snap" "sudo" ["snap", "refresh"]
  hlog "<Green>syncing Gentoo tree..."
  jobs <- jobsArg
  forM_ (syncTreeCommands jobs isGentooGitCheckout) $ \(cmd, args) ->
    rawAndIgnore "sudo" (cmd : args)
  rawAndIgnore "sudo" $ [ "emerge", "-uDN", "@world"
                        , "--backtrack=100"
                        , "--with-bdeps=y"
                        , "--quiet-build=n"
                        , "--exclude=nvidia-drivers"
                        ] ++ ["-av" | uwuAsk uws]
                          ++ ["-pv" | uwuPretend uws]

runUpgradeScripts ∷ UwuState -> Bool -> (String -> IO ()) -> IO ()
runUpgradeScripts uws isGentooGitCheckout hlog =
  isRoot (runUpgradeScriptsRoot uws isGentooGitCheckout hlog)
         (runUpgradeScriptsSudo uws isGentooGitCheckout hlog)

owo ∷ UwuState ~> m
owo uws _ = ask >>= \env -> do
  liftIO $ do
    repoPath <- readIORef (config env) >>= resolveMainRepoPath ∘ pcMakeConf
    isGentooGitCheckout <- hasGentooGithubRemote repoPath
    runUpgradeScripts uws isGentooGitCheckout (logger env)
    pc <- loadPortageConfig [UpdateMeta, PortageMeta, OverlayMeta, MiscMeta]
    writeIORef (config env) pc { pcUpdateCache = True }

uwuCmd ∷ Command UwuState m
uwuCmd = Command { command      = ["uwu"]
                 , deps         = [UpdateMeta, PortageMeta, OverlayMeta, MiscMeta]
                 , description  = "Update and upgrade the world (alias)"
                 , usage        = ("haku " ++)
                 , state        = UwuState { uwuHaskellSync = False
                                           , uwuAsk = False
                                           , uwuPretend = False }
                 , options      = uwuOpts
                 , handler      = owo }

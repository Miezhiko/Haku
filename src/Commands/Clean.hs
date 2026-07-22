module Commands.Clean
  ( cleanCmd
  ) where

import           Types
import           Utils

import           Portage.Config   (resolveDistDir, resolvePortageTmpDir)

import           System.Directory

data CleanState
  = CleanState
      { cleanDirs    :: Bool
      , cleanVerbose :: Bool
      }

cleanOpts ∷ Bool -> [OptDescr (CleanState -> CleanState)]
cleanOpts _ =
  [ Option "d" ["dist"]     (NoArg (\s -> s { cleanDirs = True }))    "clean portage DIST and TMP dirs"
  , Option "v" ["verbose"]  (NoArg (\s -> s { cleanVerbose = True })) "verbose output"
  ]

depcleanIO ∷ IO ()
depcleanIO = isRoot ( rawAndIgnore "emerge" [ "--depclean" ] )
                    ( rawAndIgnore "sudo" [ "emerge", "--depclean" ] )

cleanDirIO ∷ FilePath -> IO ()
cleanDirIO []   = pure ()
cleanDirIO path = isRoot removeDircontent
                    $ putStrLn "can't clean dirs as user"
 where
  removeDircontent ∷ IO ()
  removeDircontent = rawAndIgnore "rm" [ "-rf", path ++ "/*" ]

cleanIfExists ∷ FilePath -> IO ()
cleanIfExists p = doesDirectoryExist p >>= \exists ->
                      when exists $ cleanDirIO p

cleanM ∷ CleanState ~> m
cleanM cs _ = ask >>= \env -> liftIO $ do
  depcleanIO
  when (cleanDirs cs) $
    readIORef (config env) >>= \pc -> do
      let hlog    = logger env
          verbose = cleanVerbose cs
      distDir <- resolveDistDir pc
      tmpDir  <- resolvePortageTmpDir pc
      when verbose $ hlog ("Cleaning " ++ distDir)
      cleanIfExists distDir
      when verbose $ hlog ("Cleaning " ++ tmpDir)
      cleanIfExists tmpDir

cleanCmd ∷ Command CleanState m
cleanCmd = Command { command      = ["clean"]
                   , deps         = []
                   , description  = "Clean world"
                   , usage        = ("haku " ++)
                   , state        = CleanState { cleanDirs     = False
                                               , cleanVerbose  = False }
                   , options      = cleanOpts
                   , handler      = cleanM }

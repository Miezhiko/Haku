module Commands.Clean
  ( cleanCmd
  ) where

import           Types
import           Utils

import qualified Data.Map         as M

import           System.Directory
import           System.FilePath

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

cleanDirIO ∷ FilePath -> Bool -> IO ()
cleanDirIO [] _ = pure ()
cleanDirIO path recursive = isRoot removeDircontent
                                 $ putStrLn "can't clean dirs as user"
 where
  removeDircontent ∷ IO ()
  removeDircontent = listDirectory path
                 >>= mapM_ removeEntry

  removeEntry ∷ FilePath -> IO ()
  removeEntry name = do
    let fullPath = path </> name
    isDirectory <- doesDirectoryExist fullPath
    if isDirectory
      then when recursive $ cleanDirIO fullPath True
      else removeFile fullPath

cleanIfExists ∷ FilePath -> Bool -> IO ()
cleanIfExists p r = doesDirectoryExist p >>= \exists ->
                      when exists $ cleanDirIO p r

cleanM ∷ CleanState ~> m
cleanM cs _ = ask >>= \env -> liftIO $ do
  depcleanIO
  when (cleanDirs cs) $
    readIORef (config env) >>= \pc -> do
      let hlog     = logger env
          makeConf = pcMakeConf pc
          distDir  = makeConf M.! "DISTDIR"
          tmpDir   = makeConf M.! "PORTAGE_TMPDIR"
          verbose  = cleanVerbose cs
      when verbose $ hlog ("Cleaning " ++ distDir)
      cleanIfExists distDir False
      when verbose $ hlog ("Cleaning " ++ tmpDir)
      cleanIfExists tmpDir True

cleanCmd ∷ Command CleanState m
cleanCmd = Command { command      = ["clean"]
                   , description  = "Clean world"
                   , usage        = ("haku " ++)
                   , state        = CleanState { cleanDirs     = False
                                               , cleanVerbose  = False }
                   , options      = cleanOpts
                   , handler      = cleanM }

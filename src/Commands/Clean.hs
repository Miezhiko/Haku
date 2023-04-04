{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Commands.Clean
  ( cleanCmd
  ) where

import           Types
import           Utils

import qualified Data.Map         as M

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
cleanDirIO [] = pure ()
cleanDirIO path = isRoot
                    ( rawAndIgnore "rm" [ "-rf", insidePath ] )
                    ( rawAndIgnore "sudo" [ "rm", "-rf", insidePath ] )
 where insidePath ∷ String
       insidePath = path ++ "/*"

cleanIfExists ∷ FilePath -> IO ()
cleanIfExists p = doesDirectoryExist p >>= \exists ->
                    when exists $ cleanDirIO p

cleanM ∷ HakuMonad m ⇒ CleanState -> [String] -> m ()
cleanM cs _ = ask >>= \env -> liftIO $ do
  depcleanIO
  when (cleanDirs cs) $
    readIORef (config env) >>= \pc -> do
      let makeConf = pcMakeConf pc
          distDir  = makeConf M.! "DISTDIR"
          tmpDir   = makeConf M.! "PORTAGE_TMPDIR"
          verbose  = cleanVerbose cs
      when verbose $ putStrLn ("Cleaning " ++ distDir)
      cleanIfExists distDir
      when verbose $ putStrLn ("Cleaning " ++ tmpDir)
      cleanIfExists tmpDir

cleanCmd ∷ Command CleanState m
cleanCmd = Command { command = ["clean"]
                   , description = "Clean world"
                   , usage = ("haku " ++)
                   , state = CleanState { cleanDirs     = False
                                        , cleanVerbose  = False }
                   , options = cleanOpts
                   , handler = cleanM }

module Commands.Commit
  ( commitCmd
  ) where

import           Types
import           Utils

import           System.Directory (getCurrentDirectory)
import           System.FilePath  (isPathSeparator, takeBaseName, takeDirectory)

dropTailingPathSeparator :: FilePath -> FilePath
dropTailingPathSeparator path =
  reverse ∘ dropWhile isPathSeparator ∘ reverse $ path

commitEbuild ∷ String -> IO ()
commitEbuild c =
  let ebName  = takeBaseName c
      catName = takeBaseName $ takeDirectory $ dropTailingPathSeparator c
      catEb   = catName ++ "/" ++ ebName ++ ": "
  in do
    rawAndIgnore "git" [ "add", "." ]
    rawAndIgnore "git" [ "commit", "-asS", "-m", catEb, "-e" ]

commit ∷ IO ()
commit = getCurrentDirectory >>= commitEbuild

commitCmd ∷ Command String m
commitCmd = Command { command      = ["commit"]
                    , deps         = []
                    , description  = "commit ebuild changes current directory"
                    , usage        = ("haku " ++)
                    , state        = 𝜀
                    , options      = const 𝜀
                    , handler      = \_ _ -> liftIO commit }

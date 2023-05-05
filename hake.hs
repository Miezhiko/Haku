{-# LANGUAGE
    MultiWayIf
  , UnicodeSyntax
  #-}

import           Hake

main ∷ IO ()
main = hake $ do
  "clean | clean the project" ∫
    cabal ["clean"] ?> removeDirIfExists buildPath
                    >> cleanCabalLocal

  "build deps | install all the dependencies" ∫
    cabal ["install", "--only-dependencies", "--overwrite-policy=always"]

  hakuExecutable ♯
   let processBuild =
           cabalConfigure
        >> cabalBuild
        >> getCabalBuildPath appName >>=
            \p -> copyFile p hakuExecutable
    in processBuild ?> cleanCabalLocal

  "install | install to system" ◉ [hakuExecutable] ∰
    cabal ["install", "--overwrite-policy=always"]

  "test | build and test" ◉ [hakuExecutable] ∰
    rawSystem hakuExecutable ["--version"]
      >>= checkExitCode

 where
  appName ∷ String
  appName = "haku"

  buildPath ∷ String
  buildPath = "dist-newstyle"

  hakuExecutable ∷ String
  hakuExecutable =
    {- HLINT ignore "Redundant multi-way if" -}
    if | os ∈ ["win32", "mingw32", "cygwin32"] -> buildPath </> appName ++ "exe"
       | otherwise                             -> buildPath </> appName

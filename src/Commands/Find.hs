{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Commands.Find where

import           Types
import           Utils

import           Data.Foldable       (for_)
import           Data.List
import qualified Data.Map            as M

import           System.Console.ANSI

data FindState
  = FindState
      { fndExact :: Bool
      , fndAll   :: Bool
      }

findOpts ∷ Bool → [OptDescr (FindState → FindState)]
findOpts _ =
    [ Option "e" ["exact"] (NoArg (\s -> s { fndExact = True })) "find exact ebuild/package"
    , Option "a" ["all"] (NoArg (\s -> s { fndAll = True })) "find all!"
    ]

maybePrint ∷ Maybe Ebuild → IO ()
maybePrint Nothing   = putStrLn "no ebuild found"
maybePrint (Just eb) = putStrLn $ eDescription eb

maybePrintFind ∷ (Package, Maybe Ebuild) → IO ()
maybePrintFind (p,Nothing) = putStrLn $ show p ++ " | no ebuild found"
maybePrintFind (p,Just eb) = do
  print p
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn $ eDescription eb
  setSGR [SetColor Foreground Vivid Red]
  prettyPrintVersions $ pVersions p
  putStrLn []
  setSGR [Reset]

findAction ∷ FindState → [String] → IORef PortageConfig → IO ()
findAction _ [] _     = putStrLn "you should specify what to search!"
findAction fs [x] rpc = readIORef rpc >>= \pc ->
  if fndAll fs
    then do
      let tree = pcTree pc
          matches = M.filter (\p -> x `isInfixOf` pName p) tree
      packagesWithEbuilds <- mapM (\p -> do
                                      mbeb <- findEbuild pc p
                                      return (p, mbeb)
                                  ) matches
      for_ packagesWithEbuilds maybePrintFind
    else case findPackage pc x of
          Just p  -> do print p
                        mbeb <- findEbuild pc p
                        setSGR [SetColor Foreground Vivid Blue]
                        maybePrint mbeb
                        setSGR [SetColor Foreground Vivid Red]
                        prettyPrintVersions $ pVersions p
                        setSGR [Reset]
          Nothing -> putStrLn "Atom not found!"
findAction fs (x:xs) pc = findAction fs [x] pc
                       >> findAction fs xs pc

findM ∷ (MonadReader HakuEnv m, MonadIO m) ⇒
            FindState → [String] → m ()
findM fs xs = liftIO ∘ findAction fs xs =<< asks config

findCmd ∷ Command FindState m
findCmd = Command
          { command = ["f", "find"]
          , description = "Find some Atom in main tree and overlays"
          , usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>"
          , state = FindState { fndExact = False
                              , fndAll = False }
          , options = findOpts
          , handler = findM }

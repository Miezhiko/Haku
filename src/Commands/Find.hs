{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  , TupleSections
  #-}

module Commands.Find where

import           Types
import           Utils

import           Data.Foldable       (traverse_)
import           Data.List
import qualified Data.Map            as M
import qualified Data.Set            as S

import           System.Console.ANSI

data FindState
  = FindState
      { fndExact     :: Bool
      , fndAll       :: Bool
      , fndInstalled :: Bool
      }

findOpts ∷ Bool → [OptDescr (FindState → FindState)]
findOpts _ =
    [ Option "e" ["exact"] (NoArg (\s → s { fndExact = True })) "find exact ebuild/package"
    , Option "a" ["all"] (NoArg (\s → s { fndAll = True })) "find all (part of name)"
    , Option "i" ["installed"] (NoArg (\s → s { fndInstalled = True })) "find installed atom"
    ]

maybePrint ∷ Maybe Ebuild → IO ()
maybePrint Nothing   = putStrLn "no ebuild found"
maybePrint (Just eb) = putStrLn $ eDescription eb

maybePrintFind ∷ Bool → (Package, Maybe Ebuild) → IO ()
maybePrintFind _ (p,Nothing) = putStrLn $ show p ++ " | no ebuild found"
maybePrintFind False (p,Just eb) = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetUnderlining SingleUnderline ]
  print p
  setSGR [ SetColor Foreground Vivid Blue
         , SetUnderlining NoUnderline ]
  putStrLn $ eDescription eb
  setSGR [ SetColor Foreground Vivid Red
         , SetConsoleIntensity BoldIntensity ]
  prettyPrintVersions $ pVersions p
  putStrLn []
  setSGR [ Reset ]
maybePrintFind True (p,Just eb) =
  let versionsList = S.toList versions
      installed = any pvInstalled versionsList
  in when installed $ maybePrintFind False (p,Just eb)
 where versions ∷ S.Set PackageVersion
       versions = pVersions p

findAction ∷ FindState → [String] → IORef PortageConfig → IO ()
findAction _ [] _     = putStrLn "you should specify what to search!"
findAction fs [x] rpc = readIORef rpc >>= \pc →
  if fndAll fs
    then traverse_ (maybePrintFind (fndInstalled fs))
         =<< traverse (\p → (p,) <$> findEbuild pc p
                      ) (M.filter ((x `isInfixOf`) ∘ pName) (pcTree pc))
    else case findPackage pc x of
          Just p → do print p
                      mbeb ← findEbuild pc p
                      setSGR [SetColor Foreground Vivid Blue]
                      maybePrint mbeb
                      setSGR [SetColor Foreground Vivid Red]
                      prettyPrintVersions $ pVersions p
                      setSGR [Reset]
          Nothing → putStrLn "Atom not found!"
findAction fs (x:xs) pc = findAction fs [x] pc
                       ≫ findAction fs xs pc

findM ∷ HakuMonad m ⇒ FindState → [String] → m ()
findM fs xs = liftIO ∘ findAction fs xs =≪ asks config

findCmd ∷ Command FindState m
findCmd = Command
          { command = ["f", "find"]
          , description = "Find some Atom in main tree and overlays"
          , usage = \c → "haku " ++ c ++ " [OPTIONS] <dependency atoms>"
          , state = FindState { fndExact      = False
                              , fndAll        = False
                              , fndInstalled  = False }
          , options = findOpts
          , handler = findM }

{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Commands.Updates where

import           Types

import           Data.Foldable       (for_)
import           Data.List
import qualified Data.Map            as M
import qualified Data.Set            as S

import           System.Console.ANSI

showOnlyInstalled ∷ Package → [PackageVersion] → [PackageVersion] → IO ()
showOnlyInstalled _ _ []      = pure ()
showOnlyInstalled _ [] _      = pure ()
showOnlyInstalled p [x] vv    =
  let higherVersions = filter (> x) vv
  in unless (null higherVersions) $ do
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetUnderlining SingleUnderline ]
    print p
    setSGR [ SetColor Foreground Vivid Green
           , SetUnderlining NoUnderline
           , SetConsoleIntensity BoldIntensity ]
    putStrLn $ prettyShowVersionsList [x]
    setSGR [ SetColor Foreground Vivid Red ]
    putStrLn $ "Available: " ++ prettyShowVersionsList higherVersions
    putStrLn []
    setSGR [ Reset ]
showOnlyInstalled p xs vv  =
  let maxInstalledVersion = maximum xs
  in showOnlyInstalled p [maxInstalledVersion] vv

showSingle ∷ Package → IO ()
showSingle package =
  let versions      = pVersions package
      versionsList  = S.toList versions
      installed     = filter pvInstalled versionsList
      notInstalled  = filter (\v -> not (pvInstalled v) 
                                 && not (isLive v)
                             ) versionsList
  in showOnlyInstalled package installed notInstalled

showU ∷ IORef PortageConfig → [String] → IO ()
showU rpc [] = readIORef rpc >>= \pc →
  let tree = pcTree pc
  in for_ (M.toList tree) $ \(_, package) ->
    showSingle package
showU rpc xs = readIORef rpc >>= \pc →
  let tree = pcTree pc
  in for_ (M.toList tree) $ \(a, package) ->
    when (any (`isInfixOf` a) xs) $ showSingle package

showPossibleUpdates ∷ HakuMonad m ⇒ String → [String] → m ()
showPossibleUpdates _ xs = ask >>= \env →
   liftIO $ showU (config env) xs

updatesCmd ∷ Command String m
updatesCmd = Command
            { command = ["updates"]
            , description = "Show possible updates"
            , usage = ("haku " ++)
            , state = 𝜀
            , options = const 𝜀
            , handler = showPossibleUpdates }

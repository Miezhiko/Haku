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

isVersionMasked ∷ Version -> DepAtom -> Bool
isVersionMasked _ (DepAtom _ _ _ _ _ NoVer _) = False
isVersionMasked v (DepAtom neg _rev _dmod _ _ (DepVer vv _ast) _slt) =
  not neg && v == vv

printWithMasks ∷ [DepAtom] -> PackageVersion -> IO ()
printWithMasks masks (PackageVersion v ov installed) = do
  let findMask = find (isVersionMasked v) masks
  case findMask of
    Nothing -> setSGR [ SetColor Foreground Vivid Blue ]
    _       -> setSGR [ SetColor Foreground Vivid Red ]
  putStr $ " " ++ show v ++ "::" ++ ov
            ++ if installed
                  then " [Installed]"
                  else 𝜀
            ++ case findMask of
                  Just m  -> " [Masked " ++ show m ++ "]"
                  Nothing -> 𝜀

prettyShowVersionsListAndMasking ∷ [DepAtom] -> [PackageVersion] -> IO ()
prettyShowVersionsListAndMasking _ [] = pure ()
prettyShowVersionsListAndMasking masks vers = do
  setSGR [ SetColor Foreground Vivid Blue ]
  putStr "Available:"
  for_ vers (printWithMasks masks)
  putStrLn []

showOnlyInstalled ∷ Package
                -> [PackageVersion]
                -> [PackageVersion]
                -> [DepAtom]
                -> IO ()
showOnlyInstalled _ _ [] _      = pure ()
showOnlyInstalled _ [] _ _      = pure ()
showOnlyInstalled p [x] vv mask =
  let higherVersions = filter (> x) vv
  in unless (null higherVersions) $ do
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetUnderlining SingleUnderline ]
    print p
    setSGR [ SetColor Foreground Vivid Green
           , SetUnderlining NoUnderline
           , SetConsoleIntensity BoldIntensity ]
    putStrLn $ prettyShowVersionsList [x]
    prettyShowVersionsListAndMasking mask higherVersions
    setSGR [ Reset ]
    putStrLn []
showOnlyInstalled p xs vv mask  =
  let maxInstalledVersion = maximum xs
  in showOnlyInstalled p [maxInstalledVersion] vv mask

filteredDep ∷ String -> String -> DepAtom -> Bool
filteredDep pcat pname (DepAtom _ _ _ cat name _ _) = 
  cat == pcat && pname == name

showSingle ∷ Package -> [OverlayMeta] -> IO ()
showSingle package ovs =
  let versions      = pVersions package
      versionsList  = S.toList versions
      installed     = filter pvInstalled versionsList
      notInstalled  = filter (\v -> not (pvInstalled v) 
                                 && not (isLive v)
                             ) versionsList
      category      = pCategory package
      name          = pName package
      masksForPkg   =
        case filter (\(_, od) ->
                  any (\(c, pkgs) ->
                    c == category && name ∈ pkgs
                    ) (ovCategories od)) ovs of
          [] -> 𝜀
          xs -> let ovMasks = concatMap (\(_, od) -> ovMasking od) xs
                    mAtoms  = map mDepAtom ovMasks
                in filter (filteredDep category name) mAtoms
  in showOnlyInstalled package installed notInstalled masksForPkg

showU ∷ IORef PortageConfig -> [String] -> IO ()
showU rpc filterPackages = readIORef rpc >>= \pc ->
  let tree = pcTree pc
      ovls = M.toList (pcOverlays pc)
  in for_ (M.toList tree) $ \(a, package) ->
    case filterPackages of
      [] -> showSingle package ovls
      xs -> when (any (`isInfixOf` a) xs) $ showSingle package ovls

showPossibleUpdates ∷ HakuMonad m ⇒ String -> [String] -> m ()
showPossibleUpdates _ xs = ask >>= \env ->
   liftIO $ showU (config env) xs

updatesCmd ∷ Command String m
updatesCmd = Command
            { command = ["updates"]
            , description = "Show possible updates"
            , usage = ("haku " ++)
            , state = 𝜀
            , options = const 𝜀
            , handler = showPossibleUpdates }

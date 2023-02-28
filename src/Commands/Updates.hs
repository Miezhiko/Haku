{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , UnicodeSyntax
  #-}

module Commands.Updates where

import           Types
import           Utils

import           Data.Foldable       (for_)
import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
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

checkForEbuild ∷ PortageConfig
             -> Package
             -> PackageVersion
             -> IO (Maybe PackageVersion)
checkForEbuild pc p ver =
  findVersionedEbuild pc p ver >>=
    \case Nothing -> pure Nothing
          Just eb ->
            case eKeywords eb of
              [] -> pure Nothing    -- no keywords
              kw -> if any (∈ ["amd64", "~amd64"]) kw
                      then pure $ Just ver -- good keywords
                      else pure Nothing    -- bad keywords

showOnlyInstalled ∷ Package
                -> [PackageVersion]
                -> [PackageVersion]
                -> [DepAtom]
                -> PortageConfig
                -> IO ()
showOnlyInstalled _ _ [] _ _      = pure ()
showOnlyInstalled _ [] _ _ _      = pure ()
showOnlyInstalled p [x] vv mask pc =
  let higherVersions = filter (> x) vv
  in unless (null higherVersions) $ do
    newVersionsWithEbuilds <-
      catMaybes <$> traverse (checkForEbuild pc p) higherVersions
    unless (null newVersionsWithEbuilds) $ do
      setSGR [ SetConsoleIntensity BoldIntensity
            , SetUnderlining SingleUnderline ]
      print p
      setSGR [ SetColor Foreground Vivid Green
            , SetUnderlining NoUnderline
            , SetConsoleIntensity BoldIntensity ]
      putStrLn $ prettyShowVersionsList [x]
      prettyShowVersionsListAndMasking mask newVersionsWithEbuilds
      setSGR [ Reset ]
      putStrLn []
showOnlyInstalled p xs vv mask pc =
  let maxInstalledVersion = maximum xs
  in showOnlyInstalled p [maxInstalledVersion] vv mask pc

filteredDep ∷ String -> String -> DepAtom -> Bool
filteredDep pcat pname (DepAtom _ _ _ cat name _ _) = 
  cat == pcat && pname == name

showSingle ∷ Package -> [OverlayMeta] -> PortageConfig -> IO ()
showSingle package ovs pc =
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
  in showOnlyInstalled package installed notInstalled masksForPkg pc

showU ∷ IORef PortageConfig -> [String] -> IO ()
showU rpc filterPackages = readIORef rpc >>= \pc ->
  let tree = pcTree pc
      ovls = M.toList (pcOverlays pc)
  in for_ (M.toList tree) $ \(a, package) ->
    case filterPackages of
      [] -> showSingle package ovls pc
      xs -> when (any (`isInfixOf` a) xs) $ showSingle package ovls pc

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

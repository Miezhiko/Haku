module Commands.Updates
  ( updatesCmd
  ) where

import           Constants
import           Types
import           Utils

import           Data.Foldable       (for_)
import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S

import           System.Console.ANSI

data UpdatesState
  = UpdatesState
      { updsVerbose   :: Bool
      , updsShowMasks :: Bool
      , updsOnlyMasks :: Bool
      , updsWithBdeps :: Bool
      }

updatesOpts ∷ Bool -> [OptDescr (UpdatesState -> UpdatesState)]
updatesOpts _ =
  [ Option "v" ["verbose"]    (NoArg (\s -> s { updsVerbose = True }))    "verbose output"
  , Option "m" ["show-masks"] (NoArg (\s -> s { updsShowMasks = True }))  "show masked"
  , Option "o" ["only-masks"] (NoArg (\s -> s { updsOnlyMasks = True }))  "show only masked"
  , Option "b" ["with-bdeps"] (NoArg (\s -> s { updsWithBdeps = True }))  "show build deps updates too"
  ]

{-
showMod DNONE =  ""
showMod DLT   =  "<"
showMod DLEQ  =  "<="
showMod DEQ   =  "="
showMod DGEQ  =  ">="
showMod DGT   =  ">"
-}
isVersionMasked ∷ Version -> DepAtom -> Bool
isVersionMasked _ (DepAtom _ _ _ _ _ NoVer _)                        = True
isVersionMasked v (DepAtom _neg _rev DLT _ _ (DepVer vv _ast) _slt)  = v < vv
isVersionMasked v (DepAtom _neg _rev DLEQ _ _ (DepVer vv _ast) _slt) = v <= vv
isVersionMasked v (DepAtom _neg _rev DEQ _ _ (DepVer vv _ast) _slt)  = v == vv
isVersionMasked v (DepAtom _neg _rev DGEQ _ _ (DepVer vv _ast) _slt) = v >= vv
isVersionMasked v (DepAtom _neg _rev DGT _ _ (DepVer vv _ast) _slt)  = v > vv
isVersionMasked _ (DepAtom _neg _rev _ _ _ _ _)                      = True

printWithMasks ∷ [DepAtom] -> PackageVersion -> IO ()
printWithMasks mask (PackageVersion v ov installed) = do
  let findMask = find (isVersionMasked v) mask
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

prettyPrintVersionsListAndMasking ∷ [DepAtom]
                                 -> [PackageVersion]
                                 -> IO ()
prettyPrintVersionsListAndMasking _ [] = pure ()
prettyPrintVersionsListAndMasking masks vers = do
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

prettyPrintUpdates ∷ Package
                  -> PackageVersion
                  -> [DepAtom]
                  -> [PackageVersion]
                  -> IO ()
prettyPrintUpdates p x mask new = do
  setSGR [ SetConsoleIntensity BoldIntensity
        , SetUnderlining SingleUnderline ]
  print p
  setSGR [ SetColor Foreground Vivid Green
        , SetUnderlining NoUnderline
        , SetConsoleIntensity BoldIntensity ]
  putStrLn $ prettyShowVersionsList [x]
  prettyPrintVersionsListAndMasking mask new
  setSGR [ Reset ]
  putStrLn []

checkedPrintUpdates ∷ Package
                   -> PackageVersion
                   -> [DepAtom]
                   -> [PackageVersion]
                   -> Bool
                   -> IO ()
checkedPrintUpdates p x mask new displayMasks =
  let notMasked = if displayMasks
      then filter (\pv -> any ( isVersionMasked (pvVersion pv)
                              ) mask
                  ) new
      else filter (\pv -> not $ any ( isVersionMasked (pvVersion pv)
                                    ) mask
                  ) new
  in case notMasked of
    [] -> pure ()
    xs -> prettyPrintUpdates p x mask xs

printOnlyInstalled ∷ Package
                 -> [PackageVersion]
                 -> [PackageVersion]
                 -> [DepAtom]
                 -> PortageConfig
                 -> UpdatesState
                 -> IO ()
printOnlyInstalled _ _ [] _ _ _      = pure ()
printOnlyInstalled _ [] _ _ _ _      = pure ()
printOnlyInstalled p [x] vv mask pc uss =
  let higherVersions = filter (> x) vv
  in unless (null higherVersions) $ do
    new <- catMaybes <$> traverse (checkForEbuild pc p) higherVersions
    unless (null new) $
      if updsShowMasks uss
        then prettyPrintUpdates p x mask new
        else if updsOnlyMasks uss
          then checkedPrintUpdates p x mask new True
          else checkedPrintUpdates p x mask new False
printOnlyInstalled p xs vv mask pc uss =
  let maxInstalledVersion = maximum xs
  in printOnlyInstalled p [maxInstalledVersion] vv mask pc uss

filteredDep ∷ String -> String -> DepAtom -> Bool
filteredDep pcat pname (DepAtom _ _ _ cat name _ _) = 
  cat == pcat && pname == name

showSingle ∷ Package
          -> [OverlayMeta]
          -> MaskingData
          -> PortageConfig
          -> UpdatesState
          -> IO ()
showSingle package ovs mask pc uss =
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
          [] -> map mDepAtom (pMask mask)
          xs -> let ovMasks = concatMap (\(_, od) -> ovMasking od) xs
                in map mDepAtom (ovMasks ++ pMask mask)
      finalMasks = filter (filteredDep category name) masksForPkg
  in printOnlyInstalled package installed notInstalled finalMasks pc uss

showU ∷ IORef PortageConfig
     -> (String -> IO ())
     -> UpdatesState
     -> [String]
     -> IO ()
showU rpc hlog uss filterPackages = readIORef rpc >>= \pc -> do
  let tree = pcTree pc
      mask = pcMasking pc
      ovls = M.toList (pcOverlays pc)
  worldTree <- if updsWithBdeps uss
                then pure (M.toList tree)
                else do
                  world <- readFile constWorldFile
                  pure $ filter ( \(a, _) -> a ∈ lines world )
                                (M.toList tree)
  case worldTree of
    [] -> hlog "<Yellow>no updates available"
    wt -> for_ wt $ \(a, package) ->
            case filterPackages of
              [] -> showSingle package ovls mask pc uss
              xs -> when (any (`isInfixOf` a) xs) $
                      showSingle package ovls mask pc uss

showPossibleUpdates ∷ HakuMonad m ⇒ UpdatesState -> [String] -> m ()
showPossibleUpdates uss xs = ask >>= \env ->
   liftIO $ showU (config env) (logger env) uss xs

updatesCmd ∷ Command UpdatesState m
updatesCmd = Command
            { command     = ["updates"]
            , description = "Show possible updates"
            , usage       = ("haku " ++)
            , state       = UpdatesState { updsVerbose    = False
                                         , updsShowMasks  = False
                                         , updsOnlyMasks  = False
                                         , updsWithBdeps  = False }
            , options     = updatesOpts
            , handler     = showPossibleUpdates }

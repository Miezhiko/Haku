module Portage.Utils where

import           Prelude.Unicode

import           Portage.Config
import           Portage.Ebuild
import           Portage.Types.Package
import           Portage.Version

import           System.Directory
import           System.FilePath

import           Data.List
import qualified Data.Map              as M
import qualified Data.Set              as S

findPackageByName ∷ PortageConfig -> String -> Maybe Package
findPackageByName pc x =
  let tree = pcTree pc
      cat = find (\(c, _) -> case M.lookup (c ++ "/" ++ x) tree of
                              Just _  -> True
                              Nothing -> False
                 ) $ pcCategories pc
  in case cat of
      Just (c,_) -> findPackage pc (c ++ "/" ++ x)
      Nothing    -> Nothing

findPackage ∷ PortageConfig -> String -> Maybe Package
findPackage pc input =
  if '/' ∈ input
    then M.lookup input (pcTree pc)
    else findPackageByName pc input

findVersionedEbuild ∷ PortageConfig -> Package -> PackageVersion -> IO (Maybe Ebuild)
findVersionedEbuild pc package mv =
  let pv = pvVersion mv
      po = pvOverlay mv
      pn = pName package
      pp = pCategory package </> pn
      p  = pn ++ "-" ++ show pv ++ ".ebuild"
      od = pcOverlays pc M.! po
      op = ovFilePath od
      ep = op </> pp </> p
  in doesFileExist ep >>= parse ep
 where parse ∷ String -> Bool -> IO (Maybe Ebuild)
       parse e True  = Just <$> getEbuild e
       parse _ False = pure Nothing

findEbuild ∷ PortageConfig -> Package -> IO (Maybe Ebuild)
findEbuild pc package =
  let versions = pVersions package
  in if S.null versions
    then pure Nothing
    else let mv = maximum versions
         in findVersionedEbuild pc package mv

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
import           Data.Maybe            (isJust)
import qualified Data.Set              as S

findPackageByName ∷ PortageConfig -> String -> Maybe Package
findPackageByName pc x = do
    (c, _) <- find (\(c, _) ->
        isJust (M.lookup (c </> x) (pcTree pc))
      ) (pcCategories pc)
    findPackage pc (c </> x)

findPackage ∷ PortageConfig -> String -> Maybe Package
findPackage pc input
  | '/' ∈ input = M.lookup input (pcTree pc)
  | otherwise   = findPackageByName pc input

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
       parse e True  = Just <$> getEbuild package e
       parse _ False = pure Nothing

findEbuild ∷ PortageConfig -> Package -> IO (Maybe Ebuild)
findEbuild pc package =
  let versions = pVersions package
  in if S.null versions
    then pure Nothing
    else let mv = maximum versions
         in findVersionedEbuild pc package mv

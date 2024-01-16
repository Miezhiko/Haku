module Commands.Belongs
  ( belongsCmd
  ) where

import           Constants
import           Types

import           Data.List.Split
import qualified Data.Map            as M
import qualified Data.Set            as S

import           System.Console.ANSI
import           System.Directory
import           System.FilePath
import qualified System.IO.Strict    as Strict

onlyInstalled ∷ Package -> Maybe Package
onlyInstalled p = if S.null installed
  then Nothing
  else Just $ Package (pCategory p) installed (pName p)
 where installed = S.filter pvInstalled $ pVersions p

findContent ∷ FilePath -> String -> IO Bool
findContent f x =
  any (\l -> let splt = filter (not ∘ null) $ splitOn " " l
            in case splt of
                [ ]      -> False
                [_]      -> False
                (t:fn:_) -> (t == "obj" ∨ t == "sym") ∧ (fn == x)
      ) ∘ lines <$> Strict.readFile f

findVersions ∷ Package -> [PackageVersion] -> String -> IO [String]
findVersions _ [] _ = pure 𝜀
findVersions package [x] f =
  let path = constInstalledPath </> pCategory package
                                </> pName package ++ "-" ++ show (pvVersion x)
                                </> "CONTENTS"
  in doesFileExist path >>= parse path f
 where parse ∷ String -> String -> Bool -> IO [String]
       parse e target True  =
         findContent e target >>= \found ->
          if found then pure [show package, show x]
                   else pure 𝜀
       parse _ _ False = pure 𝜀
findVersions package (x:xs) f = do
  f1 <- findVersions package [x] f
  case f1 of
    [] -> findVersions package xs f
    ff -> pure ff

findBelongs ∷ String -> Package -> IO [String]
findBelongs f package = findVersions package versions f
 where versions ∷ [PackageVersion]
       versions = S.toList $ pVersions package

belongs ∷ String -> [String] -> IORef PortageConfig -> IO ()
belongs _ [] _ = putStrLn "you should specify what to search!"
belongs _ [x] rpc = readIORef rpc >>= \pc -> do
  let tree      = pcTree pc
      installed = M.mapMaybe onlyInstalled tree
  foundPackages <- concat <$> traverse (findBelongs x) installed
  case foundPackages of
    (p:vv:_) -> do setSGR [ SetColor Foreground Dull Green
                          , SetConsoleIntensity BoldIntensity
                          , SetUnderlining SingleUnderline ]
                   putStrLn p
                   setSGR [ SetColor Foreground Vivid Red
                          , SetUnderlining NoUnderline ]
                   putStrLn vv
                   setSGR [ Reset ]
    _ -> putStrLn "nothing found for this one"
belongs z (x:xs) pc = belongs z [x] pc
                   >> belongs z xs pc

belongsM ∷ String ~> m
belongsM s xs = liftIO ∘ belongs s xs =≪ asks config

belongsCmd ∷ Command String m
belongsCmd =
  Command { command     = ["b", "belongs"]
          , deps        = [UpdateMeta, PortageMeta, OverlayMeta, MiscMeta]
          , description = "Find owner-package for some file"
          , usage       = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>"
          , state       = 𝜀
          , options     = const 𝜀
          , handler     = belongsM }

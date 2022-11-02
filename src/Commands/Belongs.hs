{-# LANGUAGE UnicodeSyntax #-}
module Commands.Belongs where

import           Constants
import           Types

import           Data.List.Split
import qualified Data.Map            as M
import qualified Data.Set            as S

import           System.Console.ANSI
import           System.Directory
import           System.FilePath
import qualified System.IO.Strict    as Strict

onlyInstalled ∷ Package → Maybe Package
onlyInstalled p =
  let vers = pVersions p
      inst = S.filter pvInstalled vers
  in if S.null inst
    then Nothing
    else Just $ Package (pCategory p) inst (pName p)

findContent ∷ FilePath → String → IO Bool
findContent f x = do
  content <- lines <$> Strict.readFile f
  return $ any (\l -> let splt = filter (not . null) $ splitOn " " l
                      in case splt of
                         []       -> False
                         [_]      -> False
                         (t:fn:_) -> (t == "obj" || t == "sym") && (fn == x)
               ) content

findVersions ∷ Package → [PackageVersion] → String → IO [String]
findVersions _ [] _ = return []
findVersions package [x] f =
  let path = constInstalledPath </> pCategory package
                                </> pName package ++ "-" ++ show (pvVersion x)
                                </> "CONTENTS"
  in doesFileExist path >>= parse path f
 where parse ∷ String → String → Bool → IO [String]
       parse e target True  =
         findContent e target >>= \found ->
          if found then return [show package, show x]
                   else return []
       parse _ _ False = return []
findVersions package (x:xs) f = do
  f1 <- findVersions package [x] f
  case f1 of
    [] -> findVersions package xs f
    ff -> return ff

findBelongs ∷ String → Package → IO [String]
findBelongs f package = do
  let versions = S.toList $ pVersions package
  findVersions package versions f

belongs ∷ IORef PortageConfig → String → [String] → IO ()
belongs _ _ [] = putStrLn "you should specify what to search!"
belongs rpc _ [x] = readIORef rpc >>= \pc -> do
  let tree      = pcTree pc
      installed = M.mapMaybe onlyInstalled tree
  foundPackages <- concat <$> mapM (findBelongs x) installed
  case foundPackages of
    (p:vv:_)  -> do setSGR [SetColor Foreground Dull Green]
                    putStrLn p
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn vv
                    setSGR [Reset]
    _         -> putStrLn "nothing found for this one"
belongs pc z (x:xs) = do belongs pc z [x]
                         belongs pc z xs

belongsCmd ∷ Command String
belongsCmd = Command
              {
                command = ["b", "belongs"],
                description = "Find owner-package for some file",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = [],
                options = const [],
                handler = belongs
              }

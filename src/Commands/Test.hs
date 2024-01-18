module Commands.Test
  ( testCmd
  ) where

import           Types
import           Utils

import           Data.Char        (isDigit)
import           Data.List        (isSuffixOf, sortBy)

import           System.Directory (getCurrentDirectory, getDirectoryContents)
import           System.FilePath  (isPathSeparator, takeBaseName, takeDirectory)

nSort   ∷  [String] -> [String]
nSort s =  if all allFloat s then    
               let { readFloat = read ∷ String → Float } 
               in  (map show ∘ sortBy compare ∘ map readFloat) s
           else sortBy natComp s
    where allFloat  =   all (\x -> isDigit x || '.' == 'x')

natComp                                 ∷  String → String → Ordering
natComp [] []                           =   EQ
natComp [] _                            =   LT
natComp _ []                            =   GT
natComp xxs@(x:xs) yys@(y:ys)
    | noDigit x && noDigit y && x == y  =   natComp xs ys
    | noDigit x || noDigit y            =   compare x y
    | nx == ny                          =   natComp rx ry
    | otherwise                         =   compare nx ny
    where   (nx,rx)     =   getNumber xxs
            (ny,ry)     =   getNumber yys
            noDigit     =   not ∘ isDigit
            getNumber s =   let { digits = takeWhile isDigit s }
                            in (read digits ∷ Integer, drop (length digits) s)

dropTailingPathSeparator :: FilePath -> FilePath
dropTailingPathSeparator path =
  reverse ∘ dropWhile isPathSeparator ∘ reverse $ path

testEbuild ∷ String -> [String] -> IO ()
testEbuild _ []   = putStrLn "no ebuilds found"
testEbuild c [x]  =
  let ebName  = takeBaseName c
      catName = takeBaseName $ takeDirectory $ dropTailingPathSeparator c
      catEb   = catName ++ "/" ++ ebName
  in isRoot
    ( do rawAndIgnore "ebuild" [ x, "digest" ]
         rawAndIgnore "emerge" [ "-av1", catEb ] )
    ( do rawAndIgnore "sudo" [ "ebuild", x, "digest" ]
         rawAndIgnore "sudo" [ "emerge", "-av1", catEb ] )
-- maybe unneeded but maybe we want to get last version
testEbuild c xs   = testEbuild c $ tail $ nSort xs

{- HLINT ignore "Redundant <$>" -}
test ∷ IO ()
test = do
  cwd <- getCurrentDirectory
  filter (isSuffixOf ".ebuild")
      <$> (getDirectoryContents cwd)
    >>= testEbuild cwd

testCmd ∷ Command String m
testCmd = Command { command      = ["test"]
                  , deps         = []
                  , description  = "test ebuild in current directory"
                  , usage        = ("haku " ++)
                  , state        = 𝜀
                  , options      = const 𝜀
                  , handler      = \_ _ -> liftIO test }

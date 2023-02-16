{-# LANGUAGE
    DeriveGeneric
  , TupleSections
  , UnicodeSyntax
  #-}

module Portage.Types.Version where

import           GHC.Generics                  (Generic)

import           Data.Bifunctor
import           Data.Binary
import           Data.List

import           Text.ParserCombinators.Parsec

data Suffix
  = Alpha Int
  | Beta Int
  | Pre Int
  | RC Int
  | P_ Int
  deriving (Eq, Generic, Ord)

instance Binary Suffix

data Version'
  = Version' [Int] (Maybe Char) [Suffix] Int
  deriving (Eq, Ord)

data Version
  = Version [Int] (Maybe Char) [Suffix] Int String
  deriving (Generic)

instance Binary Version

instance Show Version where
  show = showVersion

instance Eq Version where
  v1 == v2 = projectVersion v1 == projectVersion v2

instance Ord Version where
  compare v1 v2 = compare (projectVersion v1) (projectVersion v2)

projectVersion ∷ Version -> Version'
projectVersion (Version ver c suf rev _) = Version' ver c suf rev

showVersion ∷ Version -> String
showVersion (Version _ _ _ _ rep) = rep

data PackageVersion
  = PackageVersion
      { pvVersion   :: Version
      , pvOverlay   :: String
      , pvInstalled :: Bool
      }
  deriving (Generic)

instance Binary PackageVersion

instance Eq PackageVersion where
  v1 == v2 = pvVersion v1 == pvVersion v2

instance Ord PackageVersion where
  compare v1 v2 = compare (pvVersion v1) (pvVersion v2)

instance Show PackageVersion where
  show = showPackageVersion

showPackageVersion ∷ PackageVersion -> String
showPackageVersion (PackageVersion v ov True) = show v ++ "::" ++ ov ++ " [Installed]"
showPackageVersion (PackageVersion v ov False) = show v ++ "::" ++ ov

parseVersion ∷ String -> Either ParseError Version
parseVersion = parse (readVersion >>= \x -> eof >> pure x) "<version number>"

readVer      ∷  CharParser st ([Int],          String)
readNum      ∷  CharParser st (Int,            String)
readC        ∷  CharParser st (Maybe Char,     String)
readSuf      ∷  CharParser st (Suffix,         String)
readSufType  ∷  CharParser st (Int -> Suffix,  String)
readSufs     ∷  CharParser st ([Suffix],       String)
readRev      ∷  CharParser st (Int,            String)

readVer      =  fmap (second (intercalate ".") . unzip) (sepBy1 readNum (char '.'))
readNum      =  do  ds <- many1 digit
                    case read ds of
                      n -> pure (n,ds)
readC        =  option (Nothing,  "")  (fmap (\x -> (Just x, [x])) letter)
readSuf      =  do  _       <- char '_'
                    (f,sr)  <- readSufType
                    (n,nr)  <- option (0, "") readNum
                    pure (f n,"_" ++ sr ++ nr)

readSufType  =  choice [ fmap (Alpha,)  (try $ string "alpha")
                       , fmap (Beta,)   (try $ string "beta" )
                       , fmap (Pre,)    (try $ string "pre"  )
                       , fmap (RC,)     (try $ string "rc"   )
                       , fmap (P_,)     (try $ string "p"    )
                       ]

readSufs     =  fmap ( second concat . unzip ) (many readSuf)
readRev      =  option (0,        "")  (  do  rr      <- string "-r"
                                              (n,nr)  <- readNum
                                              pure (n,rr ++ nr)
                                       )

readVersion ∷ CharParser st Version
readVersion =  do  (ver,  verr)  <-  readVer
                   (c,    cr  )  <-  readC
                   (suf,  sufr)  <-  readSufs
                   (rev,  revr)  <-  readRev
                   let rep = verr ++ cr ++ sufr ++ revr
                   pure $ Version ver c suf rev rep

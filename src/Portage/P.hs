module Portage.P where

import           Data.Bifunctor
import           Data.Functor.Identity

import           Control.Monad                 (void)

import           System.FilePath               ((</>))

import           Text.Parsec.Prim              hiding (try)
import           Text.ParserCombinators.Parsec

import           Portage.Types.Atom
import           Portage.Version

data P
  = P
      { catP :: Category
      , pkgP :: String
      }
  deriving (Eq, Ord, Show)

data PV
  = PV
      { catPV :: Category
      , pkgPV :: String
      , verPV :: Version
      }
  deriving (Eq, Ord, Show)

data PS
  = PS
      { catPS  :: Category
      , pkgPS  :: String
      , slotPS :: Slot
      }
  deriving (Eq, Ord, Show)

data PVS
  = PVS
      { catPVS  :: Category
      , pkgPVS  :: String
      , verPVS  :: Version
      , slotPVS :: Slot
      }
  deriving (Eq, Ord, Show)

addSlot    ∷  PV -> Slot -> PVS
addSlot (PV c p v) = PVS c p v

extractPS  ∷  PVS -> PS
extractPS (PVS c p _v s) = PS c p s

extractP   ∷  PV -> P
extractP (PV c p _v) = P c p

showPV'       ∷  PV     -> [Char]
showPV        ∷  PV     -> String
showP         ∷  P      -> String
showPS        ∷  PS     -> String
showPVS       ∷  PVS    -> String
getPV         ∷  String -> PV
getP          ∷  String -> P
showEbuildPV' ∷  PV     -> [Char]
showEbuildPV  ∷  PV     -> FilePath

showPV'        (PV _cat pkg ver)      = pkg ++ "-" ++ showVersion ver
showPV         pv@(PV cat _pkg _ver)  = cat </> showPV' pv
showEbuildPV'  pv                     = showPV' pv ++ ".ebuild"
showEbuildPV   pv@(PV cat pkg _ver)   = cat </> pkg </> showEbuildPV' pv
showP          (P cat pkg)            = cat </> pkg
showPS         (PS cat pkg slot)      = cat </> pkg ++ showSlot slot
showPVS        (PVS cat pkg ver slot) = cat </> pkg ++ "-" ++ showVersion ver ++ showSlot slot

showSlot ∷ Slot -> String
showSlot ['0'] = ""
showSlot slot  = ":" ++ slot

getPV xs      =  case parsePV xs of
                   Left   e  ->
                     error $ "getPV: cat/pkg-ver parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

getP xs       =  case parseP xs of
                   Left   e  ->
                     error $ "getCatPkg: cat/pkg parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

parsePV       ∷ [Char] -> Either ParseError PV
parsePV       =  parse (readPV >>= \x -> eof >> return x) "<cat/pkg-ver>"

readPV        ∷ Text.Parsec.Prim.ParsecT
                  [Char] st Data.Functor.Identity.Identity PV
readPV        =  do  cat         <-  readCat
                     void $ char '/'
                     (pkg,mver)  <-  readPkgAndVer
                     case mver of
                       Nothing  ->  error "readPV: version expected"
                       Just ver ->  pure (PV cat pkg ver)

parseP        ∷ [Char] -> Either ParseError P
parseP        =  parse (readP >>= \x -> eof >> return x) "<cat/pkg>"

readP         ∷ Text.Parsec.Prim.ParsecT
                 [Char] st Data.Functor.Identity.Identity P
readP         =  do  cat         <-  readCat
                     void $ char '/'
                     (pkg,mver)  <-  readPkgAndVer
                     case mver of
                       Nothing ->  pure (P cat pkg)
                       Just _  ->  error "readCatPkg: unexpected version"

readCat        ∷  CharParser st Category
readPkgAndVer  ∷  CharParser st (String, Maybe Version)

readCat        =   many1 (letter <|> digit <|> oneOf "_-")
readPkgAndVer  =   do  pre    <-  many1 (letter <|> digit <|> oneOf "_+")
                       (p, v) <-  option ("",Nothing)
                                            (do  void $ char '-'
                                                 fmap (\v -> ("", Just v)) readVerOrFail
                                                   <|> fmap (first ('-' :)) readPkgAndVer
                                            )
                       return (pre ++ p,v)

readVerOrFail  ∷  CharParser st Version
readVerOrFail  =   try $
                   do  ver    <-  many1 (letter <|> digit <|> oneOf "_+.-")
                       case parseVersion ver of
                           Left   _  -> 
                             fail "version parse error"
                           Right  x  -> pure x

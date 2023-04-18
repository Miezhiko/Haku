module Portage.Atom where

import           Portage.P
import           Portage.Types.Atom
import           Portage.Version

import           Data.Functor
import           Data.Functor.Identity

import           Text.Parsec.Prim                    hiding (try)
import           Text.ParserCombinators.Parsec       as P
import           Text.ParserCombinators.Parsec.Token

blocking ∷ DepAtom -> Bool
blocking (DepAtom b _ _ _ _ _ _) = b

unblock ∷ DepAtom -> DepAtom
block   ∷ DepAtom -> DepAtom

unblock (DepAtom _b r m c p v s) = DepAtom False r m c p v s
block   (DepAtom _b r m c p v s) = DepAtom True r m c p v s

pFromDepAtom ∷ DepAtom -> P
pFromDepAtom (DepAtom _ _ _ cat pkg _ _) = P cat pkg

pFromDepTerm ∷ DepTerm -> P
pFromDepTerm (Plain d) = pFromDepAtom d
pFromDepTerm _         = P "ERROR" "ERROR"

-- | Lifts a function on versions to a function on 'DepVer's.
liftDepVer ∷ (Version -> Version) -> (DepVer -> DepVer)
liftDepVer _ NoVer        =  NoVer
liftDepVer f (DepVer v a) =  DepVer (f v) a

-- | Returns all the atoms contained in a dependency string.
depStringAtoms ∷ DepString -> [DepAtom]
depStringAtoms = concatMap depTermAtoms
  where  depTermAtoms (Plain da)   =  [da]
         depTermAtoms (Or ds)      =  depStringAtoms ds
         depTermAtoms (And ds)     =  depStringAtoms ds
         depTermAtoms (Use _ _ dt) =  depTermAtoms dt

-- | Interprets a DepString according to given USE flags (non-negatives).
interpretDepString ∷ [UseFlag] -> DepString -> DepString
interpretDepString fs = concatMap (interpretDepTerm fs)

interpretDepTerm ∷ [UseFlag] -> DepTerm -> DepString
interpretDepTerm _  (Plain a)    =  [Plain a]
interpretDepTerm fs (Or s)       =  [Or (interpretDepString fs s)]
interpretDepTerm fs (And s)      =  [And (interpretDepString fs s)]
interpretDepTerm fs (Use b f s)
  | (f `elem` fs) /= b           =  interpretDepTerm fs s
  | otherwise                    =  []

-- | Parse a dependency atom. Takes an 'expand' function.
getDepAtom'    ∷  (String -> [Category]) -> [Char] -> DepAtom

-- | Parse a dependency atom. No 'expand' function.
getDepAtom     ∷  [Char] -> DepAtom

getDepAtom' expand da =
  case parseDepAtom expand da of
    Left   _ ->  error $ "getDepAtom: depatom parse error " ++ da
    Right  x ->  x

getDepAtom = getDepAtom' (const [])

parseDepAtom ∷ (String -> [[Char]]) -> [Char] -> Either ParseError DepAtom
parseDepAtom   expand  =  parse  (readDepAtom expand) "<depatom>"

parseDepAtoms ∷ (String -> [[Char]]) -> [Char] -> Either ParseError [DepAtom]
parseDepAtoms  expand  =  parse  (do  white
                                      many (do  d <- readDepAtom expand
                                                white
                                                pure d))
                                 "<depatoms>"

readDepAtom  ∷ (String -> [[Char]])
                     -> Text.Parsec.Prim.ParsecT
                          [Char] u Data.Functor.Identity.Identity DepAtom
readDepAtom expand
             =  do  neg         <-  optchar '!'
                    rev         <-  optchar '~'
                    dmod        <-  readDepMod
                    mcat        <-  option Nothing $ try $ do  cat <- readCat
                                                               void $ char '/'
                                                               pure (Just cat)
                    (pkg,mver)  <-  readPkgAndVer
                    cat <-  case mcat of
                              Nothing -> case expand pkg of
                                           [cat]  ->  pure cat
                                           []     ->  fail $ "unknown package: " ++ pkg
                                           cats   ->  fail $ "ambiguous name " ++ pkg ++ ", possible matches: " ++ unwords (map (\c -> c ++ "/" ++ pkg) cats)
                              Just cat -> pure cat
                    dver        <-  case mver of
                                      Nothing   ->  case (rev, dmod) of
                                                      (False,DNONE) -> pure NoVer
                                                      _ -> unexpected "absence of version"
                                      Just ver  ->  do ast <- try $ optchar '*'
                                                       pure (DepVer ver ast)
                    dslt        <-  option NoSlot $  do void $ char ':'
                                                        readSlot <&> DepSlot
                    pure (DepAtom neg rev dmod cat pkg dver dslt)

readDepMod   ∷ ParsecT [Char] u Identity DepMod
readDepMod   =  option DNONE $
                  choice $ map (\(x,s) -> fmap (const x) (try $ string s))
                            [(DLEQ,"<=")
                            ,(DLT ,"<" )
                            ,(DEQ ,"=" )
                            ,(DGEQ,">=")
                            ,(DGT ,">" )
                            ]

optchar ∷ Stream s m Char ⇒ Char -> ParsecT s u m Bool
optchar c = option False (fmap (const True) (char c))

readSlot  ∷  CharParser st Slot
readSlot  =   many1 (letter <|> digit <|> oneOf "_.-+")

-- | Read a depstring.
readDepString ∷ (String -> [Category]) -> CharParser st DepString
readDepString = many . readDepTerm

-- | Reads a depterm.
readDepTerm ∷ (String -> [Category]) -> CharParser st DepTerm
readDepTerm expand =
  choice [P.try (readUseDep expand),readGroup expand,readAtom expand]

-- | Read a depstring and handle initial whitespace.
readCompleteDepString ∷ (String -> [Category])
                          -> ParsecT [Char] u Identity DepString
readCompleteDepString expand =
    do  white
        d <- readDepString expand
        eof
        pure d

-- | Read a group or an @||@-dependency.
readGroup ∷ (String -> [Category])
               -> ParsecT [Char] u Identity DepTerm
readGroup expand =
    do  c  <-  option And (fmap (const Or) chc)
        d  <-  pars (readDepString expand)
        pure (c d)

-- | Read a dependency qualified with a use flag.
readUseDep ∷ (String -> [Category])
              -> ParsecT [Char] u Identity DepTerm
readUseDep expand =
    do  neg <- option False (fmap (const True) excl)
        use <- ident
        qmark
        thenf  <-  readDepTerm expand
        elsef  <-  option Nothing $
                     do col
                        fmap Just (readDepTerm expand)
        case elsef of
          Nothing ->  pure (Use neg use thenf)
          Just el ->  pure (And [Use neg use thenf, Use (not neg) use el])

-- an atom can also be a parenthesized depstring, which is flattened
readAtom ∷ Stream [Char] Identity t ⇒
             (String -> [[Char]]) -> ParsecT [Char] u Identity DepTerm
readAtom expand =
    do  neg  <-  option "" (fmap (const "!") excl)
        dep  <-  ident
        case parse (readDepAtom expand) "" (neg ++ dep) of
          Left pErr -> fail (show pErr)
          Right x   -> pure (Plain x)

-- | Parsec language definition for the dependency language
depstringLang ∷ GenLanguageDef [Char] u Identity
depstringLang =
    LanguageDef
        { commentStart     =  "",
          commentEnd       =  "",
          commentLine      =  "",
          nestedComments   =  False,
          identStart       =  noneOf " \t\n():?!",
          identLetter      =  noneOf " \t\n()?!",
          opStart          =  oneOf "!?():|",
          opLetter         =  oneOf "|",
          reservedNames    =  [],
          reservedOpNames  =  ["!","?","(",")",":","||"],
          caseSensitive    =  True }

depstring ∷ GenTokenParser [Char] u Identity
depstring = makeTokenParser depstringLang

ident ∷ ParsecT [Char] u Identity String
white ∷ ParsecT [Char] u Identity ()
qmark ∷ ParsecT [Char] u Identity ()
col   ∷ ParsecT [Char] u Identity ()
excl  ∷ ParsecT [Char] u Identity ()
chc   ∷ ParsecT [Char] u Identity ()
pars  ∷ ParsecT [Char] u Identity a -> ParsecT [Char] u Identity a

ident  = identifier depstring
white  = whiteSpace depstring
qmark  = reservedOp depstring "?"
col    = reservedOp depstring ":"
excl   = reservedOp depstring "!"
chc    = reservedOp depstring "||"
pars   = parens depstring

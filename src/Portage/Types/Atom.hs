module Portage.Types.Atom where

import           Portage.Types.Version

import           GHC.Generics          (Generic)

import           Data.Binary

import           System.FilePath       ((</>))

type Slot     = String
type Category = String
type UseFlag  = String

data DepAtom
  = DepAtom DepNeg DepRev DepMod Category String DepVer DepSlot
  deriving (Eq, Generic, Ord)

data DepVer
  = NoVer
  | DepVer Version DepAst
  deriving (Eq, Generic, Ord, Show)

data DepSlot
  = NoSlot
  | DepSlot Slot
  deriving (Eq, Generic, Ord, Show)

type DepNeg   =  Bool   -- ^ '!'
type DepRev   =  Bool   -- ^ '~'
type DepAst   =  Bool   -- ^ '*'
data DepMod
  = DNONE -- ^ no modifier
  | DLT -- ^ '<'
  | DLEQ -- ^ '<='
  | DEQ -- ^ '='
  | DGEQ -- ^ '>='
  | DGT -- ^ '>'
  deriving (Eq, Generic, Ord, Show)

type DepString  =  [DepTerm]
data DepTerm
  = Plain DepAtom
  | Or DepString -- || ( ... ) grouping
  | And DepString -- simple grouping
  | Use Bool UseFlag DepTerm
  deriving (Eq, Generic)

instance Binary DepAtom
instance Binary DepVer
instance Binary DepSlot
instance Binary DepMod
instance Binary DepTerm

instance Show DepTerm where
  show           =  showDepTerm
  showList xs p  =  showDepString xs ++ p

instance Show DepAtom where
  show           =  showDepAtom

showDepString ∷ [DepTerm] -> String
showDepString = unwords . map showDepTerm

showDepTerm ∷ DepTerm -> String
showDepTerm (Plain atom)              =  showDepAtom atom
showDepTerm (Or depstring)            =  "|| ( " ++ showDepString depstring ++ " )"
showDepTerm (And depstring)           =  "( " ++ showDepString depstring ++ " )"
showDepTerm (Use neg flag depterm)    =  (if neg then "!" else "")
                                         ++ flag ++ "? " ++ showDepTerm depterm

showDepAtom ∷ DepAtom -> [Char]
showDepAtom (DepAtom neg rev dmod cat pkg ver slt) = 
    (if neg then "!" else "") ++ (if rev then "~" else "") ++
    showMod dmod ++ cat </> pkg  ++ (if null sver then "" else "-") ++ sver
                                 ++ (if null sslt then "" else ":") ++ sslt
  where  sver  =  showDepVer ver
         sslt  =  showDepSlot slt

showMod ∷ DepMod -> String
showMod DNONE =  ""
showMod DLT   =  "<"
showMod DLEQ  =  "<="
showMod DEQ   =  "="
showMod DGEQ  =  ">="
showMod DGT   =  ">"

showDepVer ∷ DepVer -> String
showDepVer NoVer            =  ""
showDepVer (DepVer ver ast) =  showVersion ver ++ if ast then "*" else ""

showDepSlot ∷ DepSlot -> String
showDepSlot NoSlot        =  ""
showDepSlot (DepSlot slt) =  slt

{-# LANGUAGE UnicodeSyntax #-}
module Commands.Test where

import           Types
import           Utils

import           Data.Foldable (for_)

maybePrintTest ∷ Maybe Ebuild → IO ()
maybePrintTest Nothing    = putStrLn "no ebuild found"
maybePrintTest (Just eb)  = putStrLn $ eDescription eb

test ∷ IORef PortageConfig → String → [String] → IO ()
test rpc _ _ = readIORef rpc >>= \pc -> do
  let tree  =  pcTree pc
  ebuilds <- mapM (findEbuild pc) tree
  for_ ebuilds maybePrintTest

testCmd ∷ Command String
testCmd = Command
              {
                command = ["test"],
                description = "Merge one or more variants.",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = [],
                options = const [],
                handler = test
              }

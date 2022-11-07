{-# LANGUAGE UnicodeSyntax #-}
module Commands.Test where

import           Types
import           Utils

import           Data.Foldable (for_)

maybePrintTest ∷ (Package, Maybe Ebuild) → IO ()
maybePrintTest (p, Nothing) = putStrLn $ show p ++ " | no ebuild found"
maybePrintTest (p, Just eb) = putStrLn $ show p ++ " | " ++ eDescription eb

test ∷ IORef PortageConfig → String → [String] → IO ()
test rpc _ _ = readIORef rpc >>= \pc -> do
  let tree  =  pcTree pc
  packagesWithEbuilds <- mapM (\p -> do
                                  mbeb <- findEbuild pc p
                                  return (p, mbeb)
                              ) tree
  for_ packagesWithEbuilds maybePrintTest

testCmd ∷ Command String
testCmd = Command
          { command = ["test"]
          , description = "Test command, what it does is always different"
          , usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>"
          , state = 𝜀
          , options = const 𝜀
          , handler = test }

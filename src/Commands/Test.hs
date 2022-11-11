{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Commands.Test where

import           Types
import           Utils

import           Data.Foldable (for_)

maybePrintTest ∷ (Package, Maybe Ebuild) → IO ()
maybePrintTest (p, Nothing) = putStrLn $ show p ++ " | no ebuild found"
maybePrintTest (p, Just eb) = putStrLn $ show p ++ " | " ++ eDescription eb

test ∷ String → [String] → IORef PortageConfig → IO ()
test _ _ rpc = readIORef rpc >>= \pc -> do
  let tree  =  pcTree pc
  packagesWithEbuilds <- mapM (\p -> do
                                  mbeb <- findEbuild pc p
                                  return (p, mbeb)
                              ) tree
  for_ packagesWithEbuilds maybePrintTest

testM ∷ HakuMonad m ⇒ String → [String] → m ()
testM s xs = liftIO ∘ test s xs =<< asks config

testCmd ∷ Command String m
testCmd = Command
          { command = ["test"]
          , description = "Test command, what it does is always different"
          , usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>"
          , state = 𝜀
          , options = const 𝜀
          , handler = testM }

{-# LANGUAGE UnicodeSyntax #-}
module Commands.Test where

import           Types

testCmd ∷ Command String
testCmd = Command
              {
                command = ["test"],
                description = "Merge one or more variants.",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = [],
                options = const [],
                handler = \rpc _ _ ->
                            readIORef rpc >>= \pc -> do
                              let m = pcMakeConf pc
                              print m
              }

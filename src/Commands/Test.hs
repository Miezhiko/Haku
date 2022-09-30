{-# LANGUAGE UnicodeSyntax #-}
module Commands.Test where

import           Types

import           System.Console.GetOpt

data TestState
  = TestState
      { mpretend :: Bool
      , mupdate  :: Bool
      , mlist    :: Bool
      }

testOpts ∷ Bool → [OptDescr (TestState → TestState)]
testOpts showPrivate =
    [ Option "p" ["pretend"] (NoArg (\s -> s { mpretend = True })) "pretend"
    , Option "u" ["update"] (NoArg (\s -> s { mupdate = True })) "update variants"
    ] ++
    [ Option ""  ["list-options"] (NoArg (\s -> s { mlist = True })) "list available options"
    | showPrivate
    ]

testCmd ∷ Command TestState
testCmd = Command
              {
                command = ["test"],
                description = "Merge one or more variants.",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = TestState { mpretend = False
                                  , mupdate  = False
                                  , mlist    = False },
                options = testOpts,
                handler = \rpc ms ds -> do
                            putStrLn "AAAAAAA"
              }

{-# LANGUAGE UnicodeSyntax #-}
module Commands.Get where

import           Types
import           Utils

import           System.Console.GetOpt

data GetState
  = GetState
      { mpretend :: Bool
      , mupdate  :: Bool
      }

getOpts ∷ Bool → [OptDescr (GetState → GetState)]
getOpts _ =
    [ Option "p" ["pretend"] (NoArg (\s -> s { mpretend = True })) "pretend"
    , Option "u" ["update"] (NoArg (\s -> s { mupdate = True })) "update variants"
    ]

getCmd ∷ Command GetState
getCmd = Command
              {
                command = ["get"],
                description = "Merge one or more variants.",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = GetState { mpretend = False
                                 , mupdate  = False },
                options = getOpts,
                handler = \rpc gs ds -> do
                            if mpretend gs
                              then rawAndIgnore "emerge" ("-pav":ds)
                              else rawAndIgnore "emerge" ("-av":ds)
              }

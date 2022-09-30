{-# LANGUAGE UnicodeSyntax #-}
module Commands.Delete where

import           Types
import           Utils

import           System.Console.GetOpt

data DeleteState
  = DeleteState
      { mpretend :: Bool
      , muselessOption :: Bool
      }

deleteOpts ∷ Bool → [OptDescr (DeleteState → DeleteState)]
deleteOpts _ =
    [ Option "p" ["pretend"] (NoArg (\s -> s { mpretend = True })) "update variants"
    ]

deleteCmd ∷ Command DeleteState
deleteCmd = Command
              {
                command = ["delete"],
                description = "Delete one or more variants.",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = DeleteState { mpretend = False },
                options = deleteOpts,
                handler = \rpc dels ds -> do
                            if mpretend dels
                              then rawAndIgnore "emerge" ("-pavC":ds)
                              else rawAndIgnore "emerge" ("-avC":ds)
              }

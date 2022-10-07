{-# LANGUAGE UnicodeSyntax #-}
module Commands.Delete where

import           Types
import           Utils

data DeleteState
  = DeleteState
      { mpretend       :: Bool
      , muselessOption :: Bool
      }

deleteOpts ∷ Bool → [OptDescr (DeleteState → DeleteState)]
deleteOpts _ =
    [ Option "p" ["pretend"] (NoArg (\s -> s { mpretend = True })) "update variants"
    ]

unmerge ∷ DeleteState → [Atom] → IO ()
unmerge dels xs =
  if mpretend dels
      then rawAndIgnore "emerge" ("-pavC":xs)
      else rawAndIgnore "emerge" ("-avC":xs)

delete ∷ DeleteState → PortageConfig → [Atom] → IO ()
delete _ _ []       = putStrLn "specify atom!"
delete dels pc [x]  = case findPackage pc x of
                        Just p  -> unmerge dels [show p]
                        Nothing -> putStrLn "Atom not found!"
delete dels _ xs    = unmerge dels xs

deleteCmd ∷ Command DeleteState
deleteCmd = Command
              {
                command = ["delete"],
                description = "Delete one or more variants.",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = DeleteState { mpretend        = False
                                    , muselessOption  = False },
                options = deleteOpts,
                handler = \rpc dels ds -> readIORef rpc >>= \pc -> delete dels pc ds
              }

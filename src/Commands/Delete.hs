{-# LANGUAGE UnicodeSyntax #-}
module Commands.Delete where

import           Types
import           Utils

data DeleteState
  = DeleteState
      { dpretend  :: Bool
      , dask      :: Bool
      }

deleteOpts ∷ Bool → [OptDescr (DeleteState → DeleteState)]
deleteOpts _ =
    [ Option "p" ["pretend"]  (NoArg (\s -> s { dpretend = True }))   "update variants"
    , Option "a" ["ask"]      (NoArg (\s -> s { dask = True }))       "ask before run"
    ]

unmerge ∷ DeleteState → [Atom] → IO ()
unmerge dels xs =
  let opts =
            ["-p" | dpretend dels]
        ++  ["-a" | dask dels]
  in rawAndIgnore "emerge" (opts ++ xs)

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
                state = DeleteState { dpretend  = False
                                    , dask      = False },
                options = deleteOpts,
                handler = \rpc dels ds -> readIORef rpc >>= \pc -> delete dels pc ds
              }

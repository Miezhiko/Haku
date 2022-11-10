{-# LANGUAGE UnicodeSyntax #-}
module Commands.Delete where

import           Constants         (cosntSudoPath)
import           Types
import           Utils

import           System.Directory  (doesFileExist)
import           System.Posix.User (getRealUserID)

data DeleteState
  = DeleteState
      { dpretend :: Bool
      , dask     :: Bool
      }

deleteOpts ∷ Bool → [OptDescr (DeleteState → DeleteState)]
deleteOpts _ =
    [ Option "p" ["pretend"]  (NoArg (\s -> s { dpretend = True }))   "update variants"
    , Option "a" ["ask"]      (NoArg (\s -> s { dask = True }))       "ask before run"
    ]

{- HLINT ignore "Redundant <$>" -}
unmerge ∷ DeleteState → [Atom] → IO ()
unmerge dels xs =
  let pretend = dpretend dels
      opts = ["-C"]
          ++ ["-p" | pretend]
          ++ ["-a" | dask dels]
  in (== 0) <$> getRealUserID >>= \root ->
      if root || pretend
        then rawAndIgnore "emerge" (opts ++ xs)
        else doesFileExist cosntSudoPath >>= \sudoExists ->
              if sudoExists
                then rawAndIgnore "sudo" ("emerge" : (opts ++ xs))
                else putStrLn "should run as root or have sudo installed"

delete ∷ DeleteState → PortageConfig → [Atom] → IO ()
delete _ _ []       = putStrLn "specify atom!"
delete dels pc [x]  = case findPackage pc x of
                        Just p  -> unmerge dels [show p]
                        Nothing -> putStrLn "Atom not found!"
delete dels _ xs    = unmerge dels xs

deleteCmd ∷ Command DeleteState m
deleteCmd = Command
            { command = ["delete"]
            , description = "Delete one or more variants."
            , usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>"
            , state = DeleteState { dpretend  = False
                                  , dask      = False }
            , options = deleteOpts
            , handler = \rpc dels ds -> liftIO (readIORef rpc) >>= \pc ->
                                liftIO $ delete dels pc ds }

{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Commands.Delete where

import           Types
import           Utils

import           System.Posix.User (getRealUserID)

data DeleteState
  = DeleteState
      { dpretend :: Bool
      , dask     :: Bool
      }

deleteOpts ∷ Bool -> [OptDescr (DeleteState -> DeleteState)]
deleteOpts _ =
    [ Option "p" ["pretend"]  (NoArg (\s -> s { dpretend = True }))   "update variants"
    , Option "a" ["ask"]      (NoArg (\s -> s { dask = True }))       "ask before run"
    ]

{- HLINT ignore "Redundant <$>" -}
unmerge ∷ DeleteState -> [Atom] -> IO ()
unmerge dels xs =
  let pretend = dpretend dels
      opts = ["-C"]
          ++ ["-p" | pretend]
          ++ ["-a" | dask dels]
  in (== 0) <$> getRealUserID >>= \root ->
      if root ∨ pretend
        then rawAndIgnore "emerge" (opts ++ xs)
        else hasSudo $ rawAndIgnore "sudo" ("emerge" : (opts ++ xs))

delete ∷ DeleteState -> [Atom] -> PortageConfig -> IO ()
delete _ [] _       = putStrLn "specify atom!"
delete dels [x] pc  = case findPackage pc x of
                        Just p  -> unmerge dels [show p]
                        Nothing -> putStrLn "Atom not found!"
delete dels xs _    = unmerge dels xs

deleteM ∷ HakuMonad m ⇒ DeleteState -> [String] -> m ()
deleteM dels xs =
  liftIO ∘ ( (liftIO ∘ delete dels xs) ↢ readIORef
           ) =≪ asks config

deleteCmd ∷ Command DeleteState m
deleteCmd = Command
            { command = ["delete"]
            , description = "Delete one or more variants."
            , usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>"
            , state = DeleteState { dpretend  = False
                                  , dask      = False }
            , options = deleteOpts
            , handler = deleteM }

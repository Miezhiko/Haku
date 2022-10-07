{-# LANGUAGE UnicodeSyntax #-}
module Commands.Get where

import           Types
import           Utils

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

merge ∷ GetState → [Atom] → IO ()
merge gs xs =
  if mpretend gs
      then rawAndIgnore "emerge" ("-pav":xs)
      else rawAndIgnore "emerge" ("-av":xs)

emerge ∷ GetState → PortageConfig → [Atom] → IO ()
emerge _ _ []     = putStrLn "specify atom!"
emerge gs pc [x]  = case findPackage pc x of
                        Just p  -> merge gs [show p]
                        Nothing -> putStrLn "Atom not found!"
emerge gs _ xs    = merge gs xs

getCmd ∷ Command GetState
getCmd = Command
              {
                command = ["get"],
                description = "Merge one or more variants.",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = GetState { mpretend = False
                                 , mupdate  = False },
                options = getOpts,
                handler = \rpc gs ds -> readIORef rpc >>= \pc -> emerge gs pc ds
              }

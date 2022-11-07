{-# LANGUAGE UnicodeSyntax #-}
module Commands.Get where

import           Types
import           Utils

import           System.Directory  (doesFileExist)
import           System.Posix.User (getRealUserID)

data GetState
  = GetState
      { gpretend :: Bool
      , gupdate  :: Bool
      , gask     :: Bool
      , gbdeps   :: Bool
      , gnewuse  :: Bool
      , gdeep    :: Bool
      , gverbose :: Bool
      }

getOpts ∷ Bool → [OptDescr (GetState → GetState)]
getOpts _ =
    [ Option "p" ["pretend"]    (NoArg (\s -> s { gpretend = True }))   "pretend"
    , Option "u" ["update"]     (NoArg (\s -> s { gupdate = True }))    "update variants"
    , Option "a" ["ask"]        (NoArg (\s -> s { gask = True }))       "ask before emerge"
    , Option "w" ["with-bdeps"] (NoArg (\s -> s { gbdeps = True }))     "with build deps"
    , Option "N" ["newuse"]     (NoArg (\s -> s { gnewuse = True }))    "use new USE"
    , Option "D" ["deep"]       (NoArg (\s -> s { gdeep = True }))      "very deep"
    , Option "v" ["verbose"]    (NoArg (\s -> s { gverbose = True }))   "verbose output"
    ]

{- HLINT ignore "Redundant <$>" -}
merge ∷ GetState → [Atom] → IO ()
merge gs xs =
  let opts =
            ["-p" | gpretend gs]
        ++  ["-a" | gask gs]
        ++  ["-u" | gupdate gs]
        ++  ["-N" | gnewuse gs]
        ++  ["-D" | gdeep gs]
        ++  ["-v" | gverbose gs]
        ++  ["--with-bdeps=y" | gbdeps gs]
  in (== 0) <$> getRealUserID >>= \root ->
      if root then rawAndIgnore "emerge" (opts ++ xs)
              else rawAndIgnore "sudo" ("emerge" : (opts ++ xs))

emerge ∷ GetState → PortageConfig → [Atom] → IO ()
emerge _ _ []     = putStrLn "specify atom!"
emerge gs pc [x]  = case findPackage pc x of
                        Just p  -> merge gs [show p]
                        Nothing -> putStrLn "Atom not found!"
emerge gs _ xs    = merge gs xs

getPackage ∷ IORef PortageConfig → GetState → [String] → IO ()
getPackage rpc gs ds =
  readIORef rpc >>= \pc ->
    emerge gs pc ds

getCmd ∷ Command GetState
getCmd = Command
              {
                command = ["get"],
                description = "Merge one or more variants.",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = GetState { gpretend = False
                                 , gupdate  = False
                                 , gask     = False
                                 , gbdeps   = False
                                 , gnewuse  = False
                                 , gdeep    = False
                                 , gverbose = False },
                options = getOpts,
                handler = getPackage
              }

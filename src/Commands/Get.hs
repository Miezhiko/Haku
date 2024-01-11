module Commands.Get
  ( getCmd
  ) where

import           Types
import           Utils

import           System.Posix.User (getRealUserID)

data GetState
  = GetState
      { gpretend :: Bool
      , gupdate  :: Bool
      , gask     :: Bool
      , gbdeps   :: Bool
      , gnewuse  :: Bool
      , gdeep    :: Bool
      , goneshot :: Bool
      , gverbose :: Bool
      }

getOpts ∷ Bool -> [OptDescr (GetState -> GetState)]
getOpts _ =
  [ Option "p" ["pretend"]    (NoArg (\s -> s { gpretend = True }))   "pretend"
  , Option "u" ["update"]     (NoArg (\s -> s { gupdate = True }))    "update variants"
  , Option "a" ["ask"]        (NoArg (\s -> s { gask = True }))       "ask before emerge"
  , Option "w" ["with-bdeps"] (NoArg (\s -> s { gbdeps = True }))     "with build deps"
  , Option "N" ["newuse"]     (NoArg (\s -> s { gnewuse = True }))    "use new USE"
  , Option "D" ["deep"]       (NoArg (\s -> s { gdeep = True }))      "very deep"
  , Option "1" ["oneshot"]    (NoArg (\s -> s { goneshot = True }))   "build but dont't include in world"
  , Option "v" ["verbose"]    (NoArg (\s -> s { gverbose = True }))   "verbose output"
  ]

{- HLINT ignore "Redundant <$>" -}
merge ∷ GetState -> [Atom] -> IO ()
merge gs xs =
  let pretend = gpretend gs
      opts = ["-p" | pretend]
        ++   ["-a" | gask gs]
        ++   ["-u" | gupdate gs]
        ++   ["-N" | gnewuse gs]
        ++   ["-D" | gdeep gs]
        ++   ["-1" | goneshot gs]
        ++   ["-v" | gverbose gs]
        ++   ["--with-bdeps=y" | gbdeps gs]
  in (== 0) <$> getRealUserID >>= \root ->
    if root ∨ pretend
      then rawAndIgnore "emerge" (opts ++ xs)
      else hasSudo $ rawAndIgnore "sudo" ("emerge" : (opts ++ xs))

emerge ∷ GetState -> [Atom] -> PortageConfig -> IO ()
emerge _ [] _     = putStrLn "specify atom!"
emerge gs [x] pc  =
  if head x == '@'
    then merge gs [x]
    else case findPackage pc x of
          Just p  -> merge gs [show p]
          Nothing -> putStrLn "Atom not found!"
emerge gs xs _    = merge gs xs

getPackageM ∷ GetState ~> m
getPackageM gs xs =
  liftIO ∘ ( (liftIO ∘ emerge gs xs) ↢ readIORef
           ) =≪ asks config

getCmd ∷ Command GetState m
getCmd = Command
          { command     = ["get"]
          , deps        = [PortageMeta, OverlayMeta, MiscMeta]
          , description = "Merge one or more variants."
          , usage       = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>"
          , state = GetState  { gpretend = False
                              , gupdate  = False
                              , gask     = False
                              , gbdeps   = False
                              , gnewuse  = False
                              , gdeep    = False
                              , goneshot = False
                              , gverbose = False }
          , options = getOpts
          , handler = getPackageM }

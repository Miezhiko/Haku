{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Commands.Get where

import           Constants         (cosntSudoPath)
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
    [ Option "p" ["pretend"]    (NoArg (\s → s { gpretend = True }))   "pretend"
    , Option "u" ["update"]     (NoArg (\s → s { gupdate = True }))    "update variants"
    , Option "a" ["ask"]        (NoArg (\s → s { gask = True }))       "ask before emerge"
    , Option "w" ["with-bdeps"] (NoArg (\s → s { gbdeps = True }))     "with build deps"
    , Option "N" ["newuse"]     (NoArg (\s → s { gnewuse = True }))    "use new USE"
    , Option "D" ["deep"]       (NoArg (\s → s { gdeep = True }))      "very deep"
    , Option "v" ["verbose"]    (NoArg (\s → s { gverbose = True }))   "verbose output"
    ]

{- HLINT ignore "Redundant <$>" -}
merge ∷ GetState → [Atom] → IO ()
merge gs xs =
  let pretend = gpretend gs
      opts = ["-p" | pretend]
        ++   ["-a" | gask gs]
        ++   ["-u" | gupdate gs]
        ++   ["-N" | gnewuse gs]
        ++   ["-D" | gdeep gs]
        ++   ["-v" | gverbose gs]
        ++   ["--with-bdeps=y" | gbdeps gs]
  in (== 0) <$> getRealUserID >>= \root →
      if root ∨ pretend
        then rawAndIgnore "emerge" (opts ++ xs)
        else doesFileExist cosntSudoPath >>= \sudoExists →
              if sudoExists
                then rawAndIgnore "sudo" ("emerge" : (opts ++ xs))
                else putStrLn "should run as root or have sudo installed"

emerge ∷ GetState → [Atom] → PortageConfig → IO ()
emerge _ [] _     = putStrLn "specify atom!"
emerge gs [x] pc  = case findPackage pc x of
                        Just p  → merge gs [show p]
                        Nothing → putStrLn "Atom not found!"
emerge gs xs _    = merge gs xs

getPackageM ∷ HakuMonad m ⇒ GetState → [String] → m ()
getPackageM gs xs =
  liftIO ∘ ( (liftIO ∘ emerge gs xs) ↢ readIORef
           ) =≪ asks config

getCmd ∷ Command GetState m
getCmd = Command
          { command = ["get"]
          , description = "Merge one or more variants."
          , usage = \c → "haku " ++ c ++ " [OPTIONS] <dependency atoms>"
          , state = GetState { gpretend = False
                              , gupdate  = False
                              , gask     = False
                              , gbdeps   = False
                              , gnewuse  = False
                              , gdeep    = False
                              , gverbose = False }
          , options = getOpts
          , handler = getPackageM }

{-# LANGUAGE GADTs         #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types
  ( Command(..)
  , Command'(..)
  , module ExportedTpyes
  ) where

import           Data.IORef            as ExportedTpyes
import           Portage.Version       as ExportedTpyes
import           Portage.Config        as ExportedTpyes
import           System.Console.GetOpt as ExportedTpyes

data Command a
  = Command
      { command     :: [String]
      , description :: String
      , usage       :: String -> String
      , state       :: a
      , options     :: Bool -> [OptDescr (a -> a)]
      , handler     :: IORef PortageConfig -> a -> [String] -> IO ()
      }

data Command'
  = forall a. Command' (Command a)

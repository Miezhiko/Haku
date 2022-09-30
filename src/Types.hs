{-# LANGUAGE GADTs         #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types where

import           Data.IORef
import           System.Console.GetOpt

import           Portage.Config

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

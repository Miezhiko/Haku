{-# LANGUAGE GADTs         #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types
  ( Command (..)
  , Command' (..)
  , module ExportedTpyes
  ) where

import           Data.IORef            as ExportedTpyes
import           Portage.Config        as ExportedTpyes
import           Portage.Ebuild        as ExportedTpyes
import           Portage.Types.Package as ExportedTpyes
import           Portage.Version       as ExportedTpyes
import           Prelude.Unicode       as ExportedTpyes
import           System.Console.GetOpt as ExportedTpyes

data Command τ
  = Command
      { command     :: [String]
      , description :: String
      , usage       :: String -> String
      , state       :: τ
      , options     :: Bool -> [OptDescr (τ -> τ)]
      , handler     :: IORef PortageConfig -> τ -> [String] -> IO ()
      }

data Command'
  = ∀ τ. Command' (Command τ)

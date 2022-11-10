{-# LANGUAGE
    UnicodeSyntax
  #-}
module Portage.Types.Env
  ( module ExportedTpyes
  , HakuEnv (..)
  ) where

import           System.IO as ExportedTpyes

data HakuEnv
  = HakuEnv
      { handle :: Handle
      , logger :: String -> IO ()
      }

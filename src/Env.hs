module Env
  ( module ExportedTpyes
  , HakuEnv (..)
  , MetaData (..)
  ) where

import           Data.IORef           as ExportedTpyes
import           Portage.Types.Config as ExportedTpyes
import           Prelude.Unicode      as ExportedTpyes
import           System.IO            as ExportedTpyes

data MetaData = PortageMeta | OverlayMeta | MiscMeta

data HakuEnv
  = HakuEnv
      { handle :: Handle
      , logger :: String -> IO ()
      , config :: IORef PortageConfig
      }

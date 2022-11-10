{-# LANGUAGE
    FlexibleContexts
  , GADTs
  , RankNTypes
  , UnicodeSyntax
  #-}

module Types
  ( Command (..)
  , Command' (..)
  , module ExportedTpyes
  , liftMyAss
  ) where

import           Control.Monad.Reader  as ExportedTpyes
import           Data.IORef            as ExportedTpyes
import           Portage.Config        as ExportedTpyes
import           Portage.Ebuild        as ExportedTpyes
import           Portage.Types.Env     as ExportedTpyes
import           Portage.Types.Package as ExportedTpyes
import           Portage.Version       as ExportedTpyes
import           Prelude.Unicode       as ExportedTpyes
import           System.Console.GetOpt as ExportedTpyes

data Command τ m
  = Command
      { command :: [String]
      , description :: String
      , usage :: String -> String
      , state :: τ
      , options :: Bool -> [OptDescr (τ -> τ)]
      , handler :: (MonadReader HakuEnv m, MonadIO m) =>
                 IORef PortageConfig -> τ -> [String] -> m ()
      }

liftMyAss ∷ MonadIO m ⇒ (a → b → c → IO ()) → a → b → c → m ()
liftMyAss io rpc t xs = liftIO $ io rpc t xs

data Command'
  = ∀ τ. Command' (Command τ (ReaderT HakuEnv IO))

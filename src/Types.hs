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
import           Env                   as ExportedTpyes
import           Portage.Ebuild        as ExportedTpyes
import           Portage.Types.Package as ExportedTpyes
import           Portage.Version       as ExportedTpyes
import           System.Console.GetOpt as ExportedTpyes

data Command τ m
  = Command
      { command     :: [String]
      , description :: String
      , usage       :: String -> String
      , state       :: τ
      , options     :: Bool -> [OptDescr (τ -> τ)]
      , handler     :: (MonadReader HakuEnv m, MonadIO m) => τ -> [String] -> m ()
      }

liftMyAss ∷ MonadIO m ⇒ (α → β → IO ()) → α → β → m ()
liftMyAss io t xs = liftIO $ io t xs

data Command'
  = ∀ τ. Command' (Command τ (ReaderT HakuEnv IO))

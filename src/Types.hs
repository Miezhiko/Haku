{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , GADTs
  , RankNTypes
  , UnicodeSyntax
  #-}

module Types
  ( Command (..)
  , Command' (..)
  , module ExportedTpyes
  , HakuMonad
  ) where

import           Env                   as ExportedTpyes

import           Portage.Ebuild        as ExportedTpyes
import           Portage.Types.Atom    as ExportedTpyes
import           Portage.Types.Masking as ExportedTpyes
import           Portage.Types.Package as ExportedTpyes
import           Portage.Version       as ExportedTpyes

import           Control.Monad.Reader  as ExportedTpyes
import           Control.Monad.Unicode as ExportedTpyes

import           System.Console.GetOpt as ExportedTpyes

type HakuMonad m = (MonadReader HakuEnv m, MonadIO m)

data Command τ m
  = Command
      { command     :: [String]
      , description :: String
      , usage       :: String -> String
      , state       :: τ
      , options     :: Bool -> [OptDescr (τ -> τ)]
      , handler     :: HakuMonad m => τ -> [String] -> m ()
      }

data Command'
  = ∀ τ. Command' (Command τ (ReaderT HakuEnv IO))

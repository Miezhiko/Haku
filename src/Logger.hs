{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}

module Logger
  ( hakuLog
  , hakuLogger
  ) where

import           Types

import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (getZonedTime)

import           System.Console.ANSI

hakuLogger ∷ String → IO ()
hakuLogger msg = do
  setSGR [ SetColor Foreground Vivid Magenta ]
  putStr ∘ formatTime defaultTimeLocale "%F %T" =<< getZonedTime
  setSGR [ SetColor Foreground Dull Cyan
         , SetConsoleIntensity BoldIntensity ]
  putStr $ " " ++ msg
  setSGR [ Reset ]
  putStrLn [] -- forcing reset!

hakuLog ∷ String → HakuEnv → IO ()
hakuLog = runReaderT ∘ hLogM
 where hLogM ∷ HakuMonad m ⇒ String → m ()
       hLogM msg = liftIO ∘ flip logger msg =<< ask

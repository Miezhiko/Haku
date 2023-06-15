module Console
  ( finishProgress
  , startProgress
  ) where

import           Prelude.Unicode

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, async, cancel)
import           Control.Exception        (bracket_)
import           Control.Monad            (forM_)

import           System.Console.ANSI
import           System.Console.Terminfo  hiding (Red)
import           System.IO

runCapability ∷ Terminal -> String -> IO ()
runCapability term cap =
  forM_ (getCapability term (tiGetOutput1 cap))
        (runTermOutput term)

cursorOff, cursorOn ∷ Terminal -> IO ()
cursorOff term = runCapability term "civis"
cursorOn  term = runCapability term "cnorm"

spin ∷ IO ()
spin = forM_ (cycle "|/-\\") $ \c ->
  putChar c >> putChar '\r' >>
  hFlush stdout >> threadDelay 25000

progressIndicator ∷ String -> IO ()
progressIndicator msg = do
  hSetBuffering stdout NoBuffering
  setSGR [ SetColor Foreground Dull Red
         , SetConsoleIntensity BoldIntensity
         , SetItalicized True ]
  putStr $ "  " ++ msg
  term <- setupTermFromEnv
  putChar '\r'
  hFlush stdout
  setSGR [ SetColor Foreground Vivid Red
         , SetConsoleIntensity NormalIntensity
         , SetItalicized False ]
  bracket_ (cursorOff term)
           (cursorOn term) spin

startProgress ∷ String -> IO (Async ())
startProgress = async ∘ progressIndicator

finishProgress ∷ Async a -> IO ()
finishProgress p = cancel p
                >> setSGR [ Reset ]
                >> putStrLn []

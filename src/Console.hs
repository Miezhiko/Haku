module Console
  ( progressIndicator
  ) where

import           Control.Concurrent      (threadDelay)
import           Control.Exception       (bracket_)
import           Control.Monad           (forM_)
import           System.Console.Terminfo
import           System.IO               (hFlush, stdout)

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
  -- TODO: this way it's hard to delete | from the end?
  -- putStr $ "  " ++ msg
  putStrLn msg
  term <- setupTermFromEnv
  bracket_ (cursorOff term) (cursorOn term) spin

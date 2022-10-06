{-# LANGUAGE UnicodeSyntax #-}
module Commands.Find where

import           Types

import qualified Data.Map as M

find ∷ Tree → [Atom] → IO ()
find _ []         = putStrLn "specify atom!"
find tree [x]     = case M.lookup x tree of
                        Just p  -> do print p
                                      print (pVersions p)
                        Nothing -> putStrLn "Atom not found!"
find tree (x:xs)  = do find tree [x]
                       find tree xs

findCmd ∷ Command String
findCmd = Command
              {
                command = ["find"],
                description = "Find some",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = [],
                options = const [],
                handler = \rpc _ ds -> readIORef rpc >>= \pc -> find (pcTree pc) ds
              }

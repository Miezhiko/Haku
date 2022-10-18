{-# LANGUAGE UnicodeSyntax #-}
module Commands.Find where

import           Types
import           Utils

removeJunk :: String -> String
removeJunk xs = [ x | x <- xs, x /= '\"' ]

find ∷ PortageConfig → [Atom] → IO ()
find _ []      = putStrLn "specify atom!"
find pc [x]    = case findPackage pc x of
                      Just p  -> do print p
                                    eb <- findEbuild pc p
                                    putStrLn $ removeJunk (eDescription eb)
                                    print (pVersions p)
                      Nothing -> putStrLn "Atom not found!"
find pc (x:xs) = do find pc [x]
                    find pc xs

findCmd ∷ Command String
findCmd = Command
              {
                command = ["f", "find"],
                description = "Find some Atom in main tree and overlays",
                usage = \c -> "haku " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = [],
                options = const [],
                handler = \rpc _ ds -> readIORef rpc >>= \pc -> find pc ds
              }

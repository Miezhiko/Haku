{-# LANGUAGE UnicodeSyntax #-}
module Hacks where

import           Data.Bifunctor
import           Data.Char       (isSpace)
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M

rstrip ∷ String → String
rstrip = reverse . dropWhile isSpace . reverse

readStringMap ∷ [String] → M.Map String String
readStringMap = M.fromList . map (second tail . break (=='='))

splitOnAnyOf ∷ Eq a ⇒ [[a]] → [a] → [[a]]
splitOnAnyOf ds xs = foldl' ((. splitOn) . (>>=)) [xs] ds

align ∷ [[String]] → String
align ts =  let maxlengths = map (maximum . map length) (transpose ts)
            in  unlines . map (concat . zipWith formatIn maxlengths) $ ts
  where  formatIn ∷ Int → String → String
         formatIn n s = s ++ replicate (n - length s) ' '

ifM ∷ Monad m ⇒ m Bool → m a → m a → m a
ifM b t f = do bb <- b; if bb then t else f

anyM ∷ Monad m ⇒ (a → m Bool) → [a] → m Bool
anyM p = foldr ((||^) . p) (pure False)
 where (||^) ∷ Monad m ⇒ m Bool → m Bool → m Bool
       (||^) a = ifM a (pure True)

allM ∷ Monad m ⇒ (a → m Bool) → [a] → m Bool
allM p = foldr ((&&^) . p) (pure True)
 where (&&^) ∷ Monad m ⇒ m Bool → m Bool → m Bool
       (&&^) a b = ifM a b (pure False)

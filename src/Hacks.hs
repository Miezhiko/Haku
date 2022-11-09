{-# LANGUAGE UnicodeSyntax #-}
module Hacks where

import           Data.Bifunctor
import           Data.Char       (isSpace)
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M

-- black bird operator
-- it's a book about combinatory logic that names combinators after birds
(.:) ∷ (α → β) → (γ → δ → α) → γ → δ → β
(.:) f g x y = f (g x y)

rstrip ∷ String → String
rstrip = reverse . dropWhile isSpace . reverse

readStringMap ∷ [String] → M.Map String String
readStringMap = M.fromList . map (second tail . break (=='='))

splitOnAnyOf ∷ Eq α ⇒ [[α]] → [α] → [[α]]
splitOnAnyOf ds xs = foldl' ((. splitOn) . (>>=)) [xs] ds

align ∷ [[String]] → String
align ts =  let maxlengths = map (maximum . map length) (transpose ts)
            in  unlines . map (concat . zipWith formatIn maxlengths) $ ts
  where  formatIn ∷ Int → String → String
         formatIn n s = s ++ replicate (n - length s) ' '

ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM b t f = do bb <- b; if bb then t else f

anyM ∷ Monad m ⇒ (α → m Bool) → [α] → m Bool
anyM p = foldr ((||^) . p) (pure False)
 where (||^) ∷ Monad m ⇒ m Bool → m Bool → m Bool
       (||^) a = ifM a (pure True)

allM ∷ Monad m ⇒ (α → m Bool) → [α] → m Bool
allM p = foldr ((&&^) . p) (pure True)
 where (&&^) ∷ Monad m ⇒ m Bool → m Bool → m Bool
       (&&^) a b = ifM a b (pure False)

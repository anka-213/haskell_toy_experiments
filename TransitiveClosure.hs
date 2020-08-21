{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE NoMonomorphismRestriction #-}
module TransitiveClosure where

-- Attempt at solution to: https://stackoverflow.com/questions/19212558/transitive-closure-from-a-list-using-haskell

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type PList a = M.Map a (S.Set a)

transClosure :: Ord a => [(a,a)] -> PList a
transClosure = findFixpoint step . conv

conv :: Ord a => [(a, a)] -> PList a
conv = M.fromList . (fmap . fmap) S.singleton

findFixpoint :: Eq a => (a -> a) -> a -> a
findFixpoint f x 
  | x' == x = x
  | otherwise = findFixpoint f x'
  where
    x' = f x

step :: Ord a => PList a -> PList a
step = fmap =<< stepInner
-- step xs = fmap (stepInner xs) xs
-- step xs = M.mapWithKey (stepInner xs) xs

-- stepInner :: forall a. Ord a => PList a -> a -> S.Set a -> S.Set a
stepInner :: forall a. Ord a => PList a -> S.Set a -> S.Set a
stepInner ctx xs = S.unions (ns <$> S.elems xs) `S.union` xs
  where
    ns :: a -> Set a
    ns x = M.findWithDefault S.empty x ctx

{-
type Plist a = [(a,a)]

mkClosure :: Ord a => Plist a -> Plist a
mkClosure = inner []

inner :: Ord a => Plist a -> Plist a -> Plist a
inner ys [] = ys
inner ys ((x1,x2) : xs) = inner full xs
  where
      transleft  = [(y1, x2) | (y1, y2) <- ys, y2 == x1]
      transright = [(x1, y2) | (y1, y2) <- ys, x2 == y1]
      full = sortUniq $ (x1, x2) : transleft ++ transright ++ ys

sortUniq :: Ord a => [a] -> [a]
sortUniq = map head . group . sort

-}

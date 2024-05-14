{-# OPTIONS_GHC -Wall #-}
-- {-# LANGUAGE LambdaCase #-}
module FibonacciHeap
  (FibHeap
  , insert
  , minView
  , drawFibHeap
  , empty
  , fromList
  )
  where
import Data.IntMap (IntMap)
import Data.List (foldl')
import qualified Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Tree (Tree(..), drawForest)

-- Based on https://www.youtube.com/watch?v=6JxvKfSV9Ns&list=TLPQMTkxMDIwMjLsS-NdKXbf7g&index=2

-- Invariants: Smallest element always first
-- Invariant: After extracting smallest element, degree and length is O(log(n))
newtype FibHeap a = FH [Tree a]
 deriving Functor
 -- deriving Show

instance Show a => Show (FibHeap a) where
  show a = drawFibHeap $ fmap show a

-- todo: Implement decreasse key


drawFibHeap :: FibHeap String -> String
drawFibHeap = drawForest . unFH

unFH :: FibHeap a -> [Tree a]
unFH (FH a) = a

-- todo: Optionally enforce more invariants with types?

-- data Tree a = Node a [Tree a]
--   deriving (Show)

hd :: Tree a -> a
hd (Node a _) = a

-- TODO: Cache this
degree :: Tree a -> Int
degree (Node _ xs) = length xs

-- | put the tree with a larger head in the tree with the smaller head
--
-- >>> joinTree (Node 2 [Node 5 []]) (Node 3 [Node 4 []])
-- Node 2 [Node 3 [Node 4 []],Node 5 []]
joinTree :: Ord a => Tree a -> Tree a -> Tree a
joinTree as@(Node a trs) bs@(Node b trs')
  | a < b     = Node a (bs : trs)
  | otherwise = Node b (as : trs')

-- | insert a node in the heap
--
-- O(1)
--
-- >>> insert 3 $ FH [Node 2 [], Node 4 [pure 5]]
-- 2
-- <BLANKLINE>
-- 3
-- <BLANKLINE>
-- 4
-- |
-- `- 5
-- <BLANKLINE>
insert :: Ord a => a -> FibHeap a -> FibHeap a
insert a (FH []) = FH [Node a []]
insert a (FH rest@(Node b bs : xs))
  | a < b = FH (Node a [] : rest)
  | otherwise = FH (Node b bs : Node a [] : xs)

-- | Convert list to fibonacci heap
--
-- O(n)
fromList :: Ord a => [a] -> FibHeap a
fromList = foldr insert empty

empty :: FibHeap a
empty = FH []

-- | Extract the smallest element
--
-- Viewing the smallest is O(1).
-- Getting the tail is O(log(n))
--
-- >>> minView $ fromList [3,6,1,5,8,2,4,9]
-- Just (1,
-- 2
-- |
-- +- 3
-- |  |
-- |  `- 6
-- |
-- `- 5
-- <BLANKLINE>
-- 4
-- |
-- `- 8
-- <BLANKLINE>
-- 9
-- <BLANKLINE>
-- )
minView :: Ord a => FibHeap a -> Maybe (a, FibHeap a)
minView (FH []) = Nothing
minView (FH (Node a trs : trs2)) = Just (a, cleanup (FH (trs ++ trs2)))

-- Restore the invariants
cleanup :: Ord a => FibHeap a -> FibHeap a
cleanup = unpackAndFindMin . IntMap.elems . groupByDegree
  where
    unpackAndFindMin :: Ord a => [Tree a] -> FibHeap a
    unpackAndFindMin [] = FH []
    unpackAndFindMin (tr : trs) = FH (go (tr :| []) trs)
      where
      go :: Ord a => NonEmpty (Tree a) -> [Tree a] -> [Tree a]
      go (small :| acc) [] = small : acc
      go (small :| acc) (tr_a : trs')
        | hd small < hd tr_a = go (small :| tr_a : acc) trs'
        | otherwise = go (tr_a :| small : acc) trs'

groupByDegree :: Ord a => FibHeap a -> IntMap (Tree a)
groupByDegree = foldl' insertByDegree IntMap.empty . unFH

-- Maybe use mutable ST array instead?
insertByDegree :: Ord a => IntMap (Tree a) -> Tree a -> IntMap (Tree a)
-- insertByDegree im t = IntMap.insertWith joinTree (degree t) t im
insertByDegree im0 t0 = go t0 (degree t0) im0
  where
    -- Alternative version, not faster
    -- go t k im = case IntMap.alterF (\case Just a -> (pure a, Nothing) ; Nothing -> (Nothing, Just t)) k im of
    --     (Nothing , im') -> im'
    --     (Just a , im') -> go (joinTree a t) (k + 1) im'
    go t k im = case IntMap.updateLookupWithKey (\_ _ -> Nothing) k im of
        (Nothing , _im') -> IntMap.insert k t im -- todo: This traverses IntMap twice
        (Just a , im') -> go (joinTree a t) (k + 1) im'


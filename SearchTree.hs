{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-missing-pattern-synonym-signatures #-}

module SearchTree where

import MiniGDP

data Nat = Z | S Nat

-- data Color = B | R

data SearchTree leftBound rightBound a where
  Leaf :: SearchTree leftBound rightBound a
  Node ::
    SearchTree leftBound la a ->
    Smaller leftBound la ->
    Named la a ->
    Smaller la rightBound ->
    SearchTree la rightBound a ->
    SearchTree leftBound rightBound a

deriving instance Show a => Show (SearchTree lb rb a)

type UnboundedTree = SearchTree NegInfty Infinity

pattern Nyde a lb rb l r = Node l lb a rb r
-- retrie --adhoc "forall x lb rb l r. Nyde x lb rb l r = Node l lb x rb r" -i2 --target-file SearchTree.hs

insert :: Ord a => a -> UnboundedTree a -> UnboundedTree a
insert x = name x \nx -> insert' nx cmpNegInfty cmpInfinity
-- insert x Leaf = name x $ \nx -> Nyde nx cmpNegInfty cmpInfinity Leaf Leaf

insert' :: Ord a => Named l a -> Smaller lb l -> Smaller l rb -> SearchTree lb rb a -> SearchTree lb rb a
insert' x lb rb Leaf = Node Leaf lb x rb Leaf
insert' x lb rb (Nyde y lb' rb' l r) = case compareNamed x y of
    Left ans -> Node (insert' x lb ans l) lb' y rb' r
    Right ans -> Node l lb' y rb' (insert' x ans rb r)


-- rotate
rotateR :: SearchTree lb rb a -> SearchTree lb rb a
rotateR (Nyde x _lb rb (Nyde y llb lrb ll lr) r) = Node ll llb y (cmpTrans lrb rb) (Node lr lrb x rb r)
rotateR x = x


-- data RedBlackTree count (color :: Color) a where
--     Nil :: RedBlackTree 'Z 'B a
--     Red :: RedBlackTree n 'B a -> a -> RedBlackTree n 'B a -> RedBlackTree n 'R a
--     Black :: RedBlackTree n leftColor a -> a -> RedBlackTree n rightColor a -> RedBlackTree ('S n) 'B a

-- type RedBlackTree' a = forall count . RedBlackTree count 'B a
-- data RedBlackTree'' a = forall count . RBT ( RedBlackTree count 'B a)

-- turnBlack :: RedBlackTree n 'R a -> RedBlackTree ('S n) 'B a
-- turnBlack (Red l x r) = Black l x r

-- data Direction = Lft | Rgt

-- data RBTZipper count color a where
--     Tip :: RBTZipper count 'B a
--     -- ZB :: Direction -> a -> RedBlackTree n rightColor a -> RBTZipper ('S n) 'B a -> RBTZipper n leftColor a
--     -- ZR :: Direction -> a -> RedBlackTree n 'B a -> RBTZipper n 'R a -> RBTZipper n 'B a
--     ZLB :: a -> RedBlackTree n rightColor a -> RBTZipper ('S n) 'B a -> RBTZipper n leftColor a
--     ZLR :: a -> RedBlackTree n 'B a -> RBTZipper n 'R a -> RBTZipper n 'B a
--     ZRB :: RedBlackTree n leftColor a -> a -> RBTZipper ('S n) 'B a -> RBTZipper n rightColor a
--     ZRR :: RedBlackTree n 'B a -> a -> RBTZipper n 'R a -> RBTZipper n 'B a

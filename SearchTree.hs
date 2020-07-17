{- | An experiment to try out Ghosts of Departed Proofs

See https://hackage.haskell.org/package/gdp
and https://kataskeue.com/gdp.pdf
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-missing-pattern-synonym-signatures #-}

module SearchTree where

-- A miniature implementation of GDP to avoid external dependencies
import MiniGDP

data Nat = Z | S Nat

-- data Color = B | R

data SearchTree leftBound rightBound a where
  Leaf :: SearchTree leftBound rightBound a
  Node ::
    -- | The left subtree has this node as right bound
    SearchTree leftBound la a ->
    -- | The element must respect our left bound
    Smaller leftBound la ->
    -- | The current element is named la, so we can describe its properties with gdp
    Named la a ->
    Smaller la rightBound ->
    SearchTree la rightBound a ->
    SearchTree leftBound rightBound a

deriving instance Show a => Show (SearchTree lb rb a)

instance Foldable (SearchTree lb rb) where
    foldr _ z Leaf = z
    foldr f z (Node l _ (The x) _ r) = foldr f (x `f` (foldr f z r)) l
    foldMap _ Leaf = mempty
    foldMap f (Node l _ (The x) _ r) = foldMap f l <> f x <> foldMap f r

-- A full tree does not have any left/right bounds, which we model with magical values NegInfty/Infinity, 
-- which are smaller/larger than any other value
type UnboundedTree = SearchTree NegInfty Infinity

-- The first thing we do when inserting an element is giving it a name, so it can be referred to in the constraints
insert :: Ord a => a -> UnboundedTree a -> UnboundedTree a
insert x = name x \nx -> insert' nx cmpNegInfty cmpInfinity

-- This is very naive insertion without any balancing
-- we could do something more sofisticated and still be guaranteed that the tree is sorted as it should
insert' :: Ord a => Named l a -> Smaller lb l -> Smaller l rb -> SearchTree lb rb a -> SearchTree lb rb a
insert' x lb rb Leaf = Node Leaf lb x rb Leaf
insert' x lb rb (Node l lb' y rb' r) = case compareNamed x y of
    Left ans -> Node (insert' x lb ans l) lb' y rb' r
    Right ans -> Node l lb' y rb' (insert' x ans rb r)

-- This could be implemented mostly with automatic hole filling, without much thinking
rotateR :: SearchTree lb rb a -> SearchTree lb rb a
rotateR (Node (Node ll llb y lrb lr) _lb x rb r) = Node ll llb y (cmpTrans lrb rb) (Node lr lrb x rb r)
rotateR x = x


fromList :: Ord a => [a] -> UnboundedTree a
fromList = foldr insert Leaf

-- props:
--  toList . fromList === sort
--  fromList . toList =/= id

-- Node (Node (Node Leaf _ 3 _ (Node (Node Leaf _ 3 _ Leaf) _ 4 _ Leaf))
--            _ 5 _
--            (Node Leaf _ 5 _ Leaf))
--       _ 6 _ 
--      (Node (Node Leaf _ 7 _ Leaf) _ 8 _ Leaf)

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

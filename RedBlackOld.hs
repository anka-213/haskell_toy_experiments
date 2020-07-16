{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

data Nat = Z | S Nat
data Color = B | R

data RedBlackTree count (color :: Color) a where
    Nil :: RedBlackTree 'Z 'B a
    Red :: RedBlackTree n 'B a -> a -> RedBlackTree n 'B a -> RedBlackTree n 'R a
    Black :: RedBlackTree n leftColor a -> a -> RedBlackTree n rightColor a -> RedBlackTree ('S n) 'B a

type RedBlackTree' a = forall count . RedBlackTree count 'B a
data RedBlackTree'' a = forall count . RBT ( RedBlackTree count 'B a)

-- insert :: Ord a => a -> RedBlackTree n 'B a -> RedBlackTree ('S n) 'B a
-- insert x Nil = Black Nil x Nil
-- insert x (Black l y r)
--   | x < y = Black _ _ r

-- insert :: Ord a => a -> RedBlackTree'' a -> RedBlackTree'' a
-- insert x (RBT Nil) = RBT $ Black Nil x Nil
-- insert x (RBT (Black l y r))
--   | x < y = RBT $ Black _ y r

insertAny :: Ord a => a -> RedBlackTree n c a -> Either (RedBlackTree n 'R a) (RedBlackTree n 'B a)
insertAny x Nil = Left $ Red1 x
insertAny x (Black l y r) = let l' = either Black Black $ insertAny x l in Right $ l' y r
insertAny x (Red l y r) = let l' = either Black Black $ insertAny x l in Right $ l' y r

-- combine :: Ord a => RedBlackTree 

type N0 = 'Z
type N1 = 'S N0
type N2 = 'S N1

pattern Red1 y = Red Nil y Nil
pattern Black1 y = Black Nil y Nil

insert0 :: a -> RedBlackTree 'Z 'B a -> RedBlackTree ('S 'Z) 'B a
insert0 x Nil = Black Nil x Nil

insert1 :: Ord a => a -> RedBlackTree N1 'B a -> RedBlackTree N1 'B a
insert1 x (Black l y r)
  | x < y = case l of
	Nil -> Black (Red1 x) y r
	Red1 _ly -> case r of
		Nil -> Black l x (Red1 y) -- WRONG! (is ly < x?)
		Red Nil _ry Nil -> undefined --Red _ _ _
  | otherwise = Black Nil y (Red1 x)
-- insert1 x (Black1 y)
--   | x < y = Black (Red1 x) y Nil
--   | otherwise = Black Nil y (Red1 x)
-- insert1 x (Black (Red ll yl lr) y _) = undefined
-- insert1 x (Black _ y (Red ll yl lr)) = undefined

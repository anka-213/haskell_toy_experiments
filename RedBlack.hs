{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-missing-pattern-synonym-signatures #-}

data Nat = Z | S Nat
data Color = B | R

-- data SColor c where
--     SB :: SColor 'B
--     SR :: SColor 'R

-- data SNat n where
--     SZ :: SNat 'Z
--     SS :: SNat n -> SNat ('S n)

-- data MyRedBlackTree count color a where
--     Leaf :: MyRedBlackTree 'Z 'B a
--     Node :: MyRedBlackTree 'Z 'B a

data RedBlackTree count (color :: Color) a where
    Nil :: RedBlackTree 'Z 'B a
    Red :: RedBlackTree n 'B a -> a -> RedBlackTree n 'B a -> RedBlackTree n 'R a
    Black :: RedBlackTree n leftColor a -> a -> RedBlackTree n rightColor a -> RedBlackTree ('S n) 'B a

type RedBlackTree' a = forall count . RedBlackTree count 'B a
data RedBlackTree'' a = forall count . RBT ( RedBlackTree count 'B a)

turnBlack :: RedBlackTree n 'R a -> RedBlackTree ('S n) 'B a
turnBlack (Red l x r) = Black l x r

data Direction = Lft | Rgt

data RBTZipper count color a where
    Tip :: RBTZipper count 'B a
    ZB :: Direction -> a -> RedBlackTree n rightColor a -> RBTZipper ('S n) 'B a -> RBTZipper n leftColor a
    ZR :: Direction -> a -> RedBlackTree n 'B a -> RBTZipper n 'R a -> RBTZipper n 'B a
    -- ZLB :: a -> RedBlackTree n rightColor a -> RBTZipper ('S n) 'B a -> RBTZipper n leftColor a
    -- ZLR :: a -> RedBlackTree n 'B a -> RBTZipper n 'R a -> RBTZipper n 'B a
    -- ZRB :: RedBlackTree n leftColor a -> a -> RBTZipper ('S n) 'B a -> RBTZipper n rightColor a
    -- ZRR :: RedBlackTree n 'B a -> a -> RBTZipper n 'R a -> RBTZipper n 'B a

pattern ZLB x r z = ZB Lft x r z
pattern ZRB l x z = ZB Rgt x l z
pattern ZLR x r z = ZR Lft x r z
pattern ZRR l x z = ZR Rgt x l z

data AZipper a = forall n color. AZ (RedBlackTree n color a) (RBTZipper n color a)

withDir :: (a -> b -> a -> c) -> Direction -> a -> b -> a -> c
withDir f Lft = f
withDir f Rgt = \ l x r -> f r x l

red :: Direction -> RedBlackTree n 'B a -> a -> RedBlackTree n 'B a -> RedBlackTree n 'R a
red = withDir Red
-- black :: Direction -> RedBlackTree n leftColor a -> a -> RedBlackTree n rightColor a -> RedBlackTree ('S n) 'B a
-- black = withDir Black

fromZipper :: RedBlackTree n c a -> RBTZipper n c a -> RedBlackTree'' a
fromZipper l Tip = RBT l
fromZipper l (ZLB x r up) = fromZipper (Black l x r) up
fromZipper l (ZLR x r up) = fromZipper (Red l x r) up
fromZipper r (ZRB l x up) = fromZipper (Black l x r) up
fromZipper r (ZRR l x up) = fromZipper (Red l x r) up

fromZipper' :: AZipper a -> RedBlackTree'' a
fromZipper' (AZ l z) = fromZipper l z

goLeftR :: RedBlackTree n 'R a -> RBTZipper n 'R a -> (RedBlackTree n 'B a, RBTZipper n 'B a)
goLeftR (Red l x r) z = (l , ZLR x r z) 

goLeft :: AZipper a -> Either (RBTZipper 'Z 'B a) (AZipper a)
goLeft (AZ Nil z) = Left z
goLeft (AZ (Red l x r) z) = Right $ AZ l (ZLR x r z)
goLeft (AZ (Black l x r) z) = Right $ AZ l (ZLB x r z)

goRight :: AZipper a -> Either (RBTZipper 'Z 'B a) (AZipper a)
goRight (AZ Nil z) = Left z
goRight (AZ (Red l x r) z) = Right $ AZ r (ZRR l x z)
goRight (AZ (Black l x r) z) = Right $ AZ r (ZRB l x z)

goUp :: AZipper a -> Maybe (AZipper a)
goUp (AZ _here Tip) = Nothing
goUp (AZ l (ZLB x r up)) = Just $ AZ (Black l x r) up
goUp (AZ l (ZLR x r up)) = Just $ AZ (Red l x r) up
goUp (AZ r (ZRB l x up)) = Just $ AZ (Black l x r) up
goUp (AZ r (ZRR l x up)) = Just $ AZ (Red l x r) up

mkAZipper :: RedBlackTree n 'B a -> AZipper a
mkAZipper = flip AZ Tip

stepDown :: Ord a => a -> AZipper a -> Either (RBTZipper 'Z 'B a) (AZipper a)
stepDown _ (AZ Nil z) = Left z
stepDown x0 (AZ (Red l x r) z)
  | x0 < x    = Right $ AZ l (ZLR x r z)
  | otherwise = Right $ AZ r (ZRR l x z)
stepDown x0 (AZ (Black l x r) z) 
  | x0 < x    = Right $ AZ l (ZLB x r z)
  | otherwise = Right $ AZ r (ZRB l x z)
-- stepDown x z@(AZ (Red _ y _) _)   | x < y = goLeft z | otherwise = goRight z
-- stepDown x z@(AZ (Black _ y _) _) | x < y = goLeft z | otherwise = goRight z

loopTillLeft :: (a -> Either b a) -> a -> b  
-- loopTillLeft f x = either id (loopTillLeft f) $ f x
loopTillLeft f = go where go = either id go . f

findBottom :: Ord a => a -> RedBlackTree n 'B a -> RBTZipper 'Z 'B a
findBottom x = loopTillLeft (stepDown x) . mkAZipper

-- A "BadZipper" is a red node with a zipper chain that wants a black node
data BadZipper a = forall n. BZ (RedBlackTree n 'R a) (RBTZipper n 'B a)

mkBad :: a -> RBTZipper 'Z 'B a -> BadZipper a
mkBad a z = BZ (Red Nil a Nil) z

-- Cases from here: https://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Insertion

tryInsert :: RedBlackTree n 'R a -> RBTZipper n 'B a -> Either (AZipper a) (BadZipper a)
tryInsert l Tip = Left $ AZ (turnBlack l) Tip -- Case 1
tryInsert l (ZB d x r z) = Left $ AZ l (ZB d x r z) -- Case 2
tryInsert l (ZLR x r (ZLB x1 r1@(Red {}) z)) = -- Case 3
  let l' = Black l x r
      r' = turnBlack r1
      node = Red l' x1 r'
      in Right $ BZ node z
tryInsert n1 (ZLR x2 n3 (ZLB x4 n5@(Black {}) z)) = -- Case 4b
      Left $ AZ (Black n1 x2 (Red n3 x4 n5)) z
-- Todo:
-- * Case 4a
-- * Mirrored versions


tryInsert' :: BadZipper a -> Either (AZipper a) (BadZipper a)
tryInsert' (BZ l a) = tryInsert l a

repairBad :: BadZipper a -> AZipper a
repairBad = loopTillLeft tryInsert'

insert :: Ord a => a -> RedBlackTree' a -> RedBlackTree'' a
insert x = fromZipper' . repairBad . mkBad x . findBottom x

-- insert :: Ord a => a -> RedBlackTree n 'B a -> RedBlackTree ('S n) 'B a
-- insert x Nil = Black Nil x Nil
-- insert x (Black l y r)
--   | x < y = Black _ _ r

-- insert :: Ord a => a -> RedBlackTree'' a -> RedBlackTree'' a
-- insert x (RBT Nil) = RBT $ Black Nil x Nil
-- insert x (RBT (Black l y r))
--   | x < y = RBT $ Black _ y r

-- insertAny :: Ord a => a -> RedBlackTree n c a -> Either (RedBlackTree n 'R a) (RedBlackTree n 'B a)
-- insertAny x Nil = Left $ Red1 x
-- insertAny x (Black l y r) = let l' = either Black Black $ insertAny x l in Right $ l' y r
-- insertAny x (Red (l@Black {}) y (r@Black {})) = let l' = either _ Red $ insertAny x l in Left $ l' y r
-- -- insertAny x (Red l y r) = let l' = either _ Red $ insertAny x l in Left $ l' y r

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
    Red Nil _ly Nil -> case r of
        Nil -> Black l x (Red1 y) -- WRONG! (is ly < x?)
        Red Nil _ry Nil -> undefined --Red _ _ _
  | otherwise = Black Nil y (Red1 x)
-- insert1 x (Black1 y)
--   | x < y = Black (Red1 x) y Nil
--   | otherwise = Black Nil y (Red1 x)
-- insert1 x (Black (Red ll yl lr) y _) = undefined
-- insert1 x (Black _ y (Red ll yl lr)) = undefined
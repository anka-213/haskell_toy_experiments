module Blueprint where

-- import Test.LeanCheck



import Prelude hiding (head, map)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- instance Listable a => Listable (List a) where
--   tiers = cons0 Nil \/ cons2 Cons

fold :: r -> (a -> r -> r) -> List a -> r
fold nil cons xs = case xs of
  Nil -> nil
  Cons x xs' -> cons x (fold nil cons xs')

tails :: List a -> List (List a)
tails =
  fold (Cons Nil Nil) ( \ x y -> Cons (Cons x (head y)) y )

prop :: Eq a => List a -> Bool
prop = \ xs -> head (tails xs) == xs

head :: List a -> a
-- partielle Funktion!
head (Cons x xs) = x

test :: Bool
test = and
  [ tails (Cons 1 (Cons 2 Nil))
    == Cons (Cons 1 (Cons 2 Nil)) (Cons (Cons 2 Nil) (Cons Nil Nil))
  -- , holds 100 (prop @Bool)
  ]

-- >>> tails (Cons 1 (Cons 2 Nil))

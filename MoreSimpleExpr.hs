#!/usr/bin/env stack
-- stack script --snapshot lts-21.25 --package simple-expr
-- {-# #-}
{-# LANGUAGE PatternSynonyms #-}

import Debug.SimpleExpr
import Debug.SimpleExpr.Expr
import Data.Fix
import Data.Functor.Classes
import Data.List (sort, groupBy)
import Data.Function (on)
import qualified Data.Map as M

a :: SimpleExpr = variable "a"
b :: SimpleExpr = variable "b"
-- b :: SimpleExpr = Fix (VariableF "b")
c :: SimpleExpr = variable "c"
k :: SimpleExpr = variable "k"
z :: SimpleExpr = variable "z"
lc :: SimpleExpr = variable "log c" -- Todo, use function instead
lk :: SimpleExpr = variable "log k"
log_k :: SimpleExpr = variable "log k"
log_c :: SimpleExpr = variable "log c"
o_2 :: SimpleExpr = variable "0.2"

mySin = unaryFunc "sin"
(-*-) = binaryFunc "-*-"

pattern a :+ b = Fix (BinaryFuncF "+" a b)
infixl 9 :+
pattern a :* b = Fix (BinaryFuncF "·" a b)
infixl 9 :*
pattern a :- b = Fix (BinaryFuncF "-" a b)
infixl 9 :-
pattern Zero = Fix (NumberF 0)

pattern a :+: b = BinaryFuncF "+" (Fix a) (Fix b)
pattern a :*: b = BinaryFuncF "·" (Fix a) (Fix b)
pattern a :-: b = BinaryFuncF "-" (Fix a) (Fix b)

simplify' :: Fix SimpleExprF -> Fix SimpleExprF
simplify' = bottomUp (unFix . simplifyStep id . Fix)

bottomUp :: Functor f => (f (Fix g) -> g (Fix g)) -> Fix f -> Fix g
bottomUp nt = go where go = Fix . nt . fmap go . unFix

pattern Nr :: Integer -> Fix SimpleExprF
pattern Nr n = Fix (NumberF n)

bottomUp' :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
bottomUp' nt = go where go = nt . Fix . fmap go . unFix

simplify'' :: SimpleExpr -> SimpleExpr
simplify'' = bottomUp' simpStep

simpStep :: SimpleExpr -> SimpleExpr
simpStep e = case e of
    0 :+ y -> y
    x :+ 0 -> x
    Nr a :+ Nr b -> Nr (a + b)

    0 :- y -> negate y
    x :- 0 -> x
    Nr n :- Nr m -> Nr (n - m)
    x :- y | x == y -> 0

    0 :* _ -> 0
    _ :* 0 -> 0
    1 :* y -> y
    x :* 1 -> x

    _ -> e

pattern FSoPF xss = Fix (SoPF xss)

toSoPF :: SimpleExprF SoP -> SoPF SoP
toSoPF e = case e of
    (SoPF as) :+: (SoPF bs) -> SoPF $ mergeTerms $ sort (as ++ bs) -- TODO: Use merge for faster sort and merge equal elems
    (SoPF as) :*: (SoPF bs) -> SoPF [(sort $ a ++ b, n * m) | (a, n) <- as, (b, m) <- bs]
    NumberF 0 -> SoPF []
    NumberF n -> SoPF [([],n)]
    _ -> SoPF [([e],1)]
  where
    mergeTerms :: [([SimpleExprF SoP], Integer)] -> [([SimpleExprF SoP], Integer)]
    mergeTerms = id -- _ . groupBy ((==) `on` fst)


toSoP :: SimpleExpr -> SoP
toSoP = bottomUp toSoPF

-- >>> toSoP $ (0 + 1) + mySin (a + 0 + b)*2 + 3*k*4 + 3 + 5 +
-- 1 + 2*sin (a + b) + 12*k + 3 + 5

-- Expression as a sum of products
newtype SoPF a = SoPF [([SimpleExprF a], Integer)] deriving (Eq, Ord, Functor)
type SoP = Fix SoPF
-- SoPF [([a,b],n),([c,d], m)] ~ n*a*b + m*c*d
-- Invariants for SoPF [(a,n)]:
-- n /= 0
-- a /= NumberF _
-- a /= BinaryFuncF "+" _ _
-- a /= BinaryFuncF "·" _ _

instance Eq1 SoPF where
  -- liftEq innerEq (SoPF xss) (SoPF yss) = _
  liftEq = liftEqAuto

instance Ord1 SoPF where
  -- liftCompare innerCompare (SoPF xss) (SoPF yss) = liftCompare (liftCompare2 (liftCompare (liftCompare innerCompare)) compare) xss yss
  liftCompare = liftCompareAuto

liftEqAuto :: (Functor f, Eq (f (EqWrapper a b))) => (a -> b -> Bool) -> f a -> f b -> Bool
liftEqAuto cmp fa fb = fmap (EqWrapper cmp . Left) fa == fmap (EqWrapper cmp . Right) fb

liftCompareAuto :: (Functor f, Ord (f (OrdWrapper a b))) => (a -> b -> Ordering) -> f a -> f b -> Ordering
liftCompareAuto cmp fa fb = compare (fmap (OrdWrapper cmp . Left) fa) (fmap (OrdWrapper cmp . Right) fb)

data EqWrapper a b = EqWrapper (a -> b -> Bool) (Either a b)
data OrdWrapper a b = OrdWrapper (a -> b -> Ordering) (Either a b)

instance Eq (EqWrapper a b) where
  (==) (EqWrapper cmp (Left a)) (EqWrapper _ (Right b)) = cmp a b
  (==) _ _ = error "liftEqAuto: Invalid comparison"

instance Eq (OrdWrapper a b) where
  a == b = compare a b == EQ
instance Ord (OrdWrapper a b) where
  compare (OrdWrapper cmp (Left a)) (OrdWrapper _ (Right b)) = cmp a b
  compare _ _ = error "liftCompareAuto: Invalid comparison"

newtype ShowExpr a = SE (SimpleExprF a)

data ShowPair a = SP { getLSP :: Int -> a -> ShowS, getLSL :: [a] -> ShowS }

liftShowPair :: Show1 f => ShowPair a -> ShowPair (f a)
liftShowPair (SP sp sl) = SP (liftShowsPrec sp sl) (liftShowList sp sl)

showsPrecOperList :: Int -> String -> String -> (Int -> a -> ShowS) -> Int -> [a] -> ShowS
showsPrecOperList prec name def sp d xs = case xs of
    [] -> showString def
    [x] -> sp d x
    _ -> showParen (d > prec) $ foldr1 (\s r -> s . showString name . r) $ fmap (sp (prec + 1)) xs


-- instance Show a => Show (ShowExpr a) where showsPrec d (SE e) = case e of { NumberF n -> showsPrec d n; VariableF nm -> showString nm; BinaryFuncF nm l r -> showParen (d > 9) $ showsPrec 10 l . showString " " . showString nm . showString " " . showsPrec 10 r; SymbolicFuncF name args -> showParen (d > 10) $ showString name . foldr (.) id [showString " " . showsPrec 11 arg | arg <- args] }
instance Show a => Show (ShowExpr a) where showsPrec d (SE e) = liftShowsPrec showsPrec showList d e

instance Show1 SimpleExprF where
  liftShowsPrec showsPrec' _ d e = case e of
    NumberF n -> showsPrec d n
    VariableF nm -> showString nm
    BinaryFuncF nm l r -> showParen (d > 1) $ showsPrec' 10 l . showString " " . showString nm . showString " " . showsPrec' 10 r
    SymbolicFuncF name args -> showParen (d > 10) $ showString name . foldr (.) id [showString " " . showsPrec' 11 arg | arg <- args]

-- instance Show1 SoPF where liftShowsPrec sp sl d (SoPF e) = liftShowsPrec (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) (liftShowList (liftShowsPrec sp sl) (liftShowList sp sl)) d e
-- instance Show1 SoPF where liftShowsPrec sp sl d (SoPF e) = (getLSP . liftShowPair . liftShowPair . liftShowPair $ SP sp sl) d e
instance Show1 SoPF where
  liftShowsPrec sp sl d (SoPF e) = showPlus (showTimes (liftShowsPrec sp sl)) d e
    where
      showPlus  = showsPrecOperList 6 " + " "0"
      showTimes sp d (xs, 1) = showsPrecOperList 7 "*" "1" sp d xs
      showTimes sp d (xs, n) = showsPrecOperList 7 "*" "1" sp d (NumberF n : xs)

-- Remove the "Fix" wrapper
-- instance {-# OVERLAPPING #-} Show (Fix SoPF) where showsPrec d (Fix f) = showsPrec d f
instance {-# OVERLAPPING #-} Show (Fix SoPF) where showsPrec d (Fix f) = liftShowsPrec showsPrec showList d f

deriving instance Ord a => Ord (SimpleExprF a)
instance {-# OVERLAPPING #-} Show (Fix ShowExpr) where showsPrec d (Fix f) = showsPrec d f

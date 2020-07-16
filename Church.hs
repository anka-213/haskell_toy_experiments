{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

import Data.Foldable

-- See this question: https://stackoverflow.com/questions/32288370/more-efficient-tail-of-church-encoded-list

newtype ChurchList a = CList {runCList :: forall r. (a -> r -> r) -> r -> r}
  deriving (Functor)

repeatCL :: a -> ChurchList a
repeatCL x = CList \c n -> let loop = c x loop in loop

listToCL :: Foldable t => t a -> ChurchList a
listToCL xs = CList \c n -> foldr c n xs

clToList :: ChurchList a -> [a]
clToList = toList
-- clToList cl = runCList cl (:) []

instance Show a => Show (ChurchList a) where
    show x = "(listToCL " ++ show (clToList x) ++ ")"

instance Foldable ChurchList where
    foldr :: (a -> r -> r) -> r -> ChurchList a -> r
    foldr c n cl = runCList cl c n

tailcl :: ChurchList a -> Maybe (ChurchList a)
tailcl = fmap snd . foldr (\x ->
        Just . (,) x . maybe nilcl (\(x', xs') -> conscl x' xs')) Nothing
-- tailcl = fmap snd . foldcl (\x -> Just . (,) x . \case Nothing -> nilcl; Just (x', xs') -> conscl x' xs') Nothing
-- tailcl = fmap snd . foldcl (\x xs ->
        -- Just . (,) x $ CList \c n -> case xs of Nothing -> n; Just (x', xs') -> c x' $ runCList xs' c n) Nothing
        -- Just . (,) x $ CList \c n -> case xs of Nothing -> n; Just (x', xs') -> c x' $ runCList xs' c n) Nothing
        -- Just . (,) x $ case xs of Nothing -> nilcl; Just (x', xs') -> conscl x' xs') Nothing
        -- Just $ case xs of Nothing -> (x, nilcl); Just (x', xs') -> (x, conscl x' xs')) Nothing

newtype MaybeCL a = Mcl (forall r. (r -> (a -> r) -> r))

nothingCL :: MaybeCL a
nothingCL = Mcl \ n _ -> n

justCL :: a -> MaybeCL a
justCL x = Mcl \ _ j -> j x

maybeCL :: r -> (a -> r) -> MaybeCL a -> r
maybeCL n j (Mcl f) = f n j

instance Functor MaybeCL where
    fmap f (Mcl x) = Mcl \n j -> x n $ j . f



tailcl' :: ChurchList a -> MaybeCL (ChurchList a)
-- tailcl' = fmap snd . foldr _ nothingCL
tailcl' = fmap snd . foldr (\x -> justCL . (,) x . maybeCL nilcl (\(x', xs') -> conscl x' xs')) nothingCL
-- tailcl' = fmap snd . foldr (\x xs -> _) nothingCL

type MaybeCL' a = forall r. (r -> (a -> r) -> r)

-- mapMCL' :: (a -> b) -> MaybeCL' a -> MaybeCL' b
mapMCL' :: (a1 -> b) -> (a2 -> (a1 -> c1) -> c2) -> a2 -> (b -> c1) -> c2
mapMCL' = (.) . flip (.) . flip (.)

-- justCL' :: a -> r -> (a -> r) -> r
justCL' :: a -> MaybeCL' a
justCL' x _ j = j x

maybeCL' :: r -> (a -> r) -> MaybeCL' a -> r
maybeCL' n j f = f n j

-- tailcl'' :: ChurchList a -> MaybeCL' (ChurchList a)
-- tailcl'' = mapMCL' snd . foldr (\x xs -> justCL' . (,) x . maybeCL' _ _) const
-- tailcl'' = ((.) . flip (.) . flip (.)) snd . foldr (\x -> justCL' . (,) x . _) const
-- tailcl'' = mapMCL' snd . foldr (\x -> justCL' . (,) x . _) const
-- tailcl'' = mapMCL' snd . foldr (\x -> justCL . (,) x . maybeCL nilcl (\(x', xs') -> conscl x' xs')) nothingCL

class ListLike l where
    nil :: l a
    cons :: a -> l a -> l a

instance ListLike ChurchList where
    nil = nilcl
    cons = conscl

nilcl :: ChurchList a
nilcl = CList \c n -> n

conscl :: a -> ChurchList a -> ChurchList a
conscl x xs = CList \c n -> c x $ runCList xs c n

appendcl :: ChurchList a -> ChurchList a -> ChurchList a
appendcl xs ys = CList \c n -> runCList xs c $ runCList ys c n

-- fmap (take 10 . clToList ) $ tailcl $ listToCL $ [1..12] ++ undefined

-- | Scott encoded list (Source: https://kseo.github.io/posts/2016-12-13-scott-encoding.html)
newtype ListS a =
   ListS {
     unconsS :: forall r. (a -> ListS a -> r) -> r -> r
   }

{-
-- Simple Scott
cons = (\ h t c n -> c h t)
nil = (\ c n -> n)
-}

unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unconsS' f z xs = unconsS xs f z

instance Foldable ListS where
    foldr = foldrS

foldrS :: (a -> b -> b) -> b -> ListS a -> b
foldrS f z = go
  where go = unconsS' (\x -> f x . go) z

scottToChurch :: ListS a -> ChurchList a
scottToChurch = listToCL

instance ListLike ListS where
    nil = ListS \_ z -> z
    cons x xs = ListS \c _ -> c x xs

-- toScott :: ChurchList a -> ListS a
-- toScott :: (Foldable t, ListLike l) => t a -> l a
toScott :: Foldable t => t a -> ListS a
toScott = foldr cons nil

convert :: (Foldable t, ListLike l) => t a -> l a
convert = foldr cons nil

-- toScott' :: ChurchList a -> ListS a
-- toScott' xs = ListS \c z -> let go = runCList xs (\x xs -> c x (toScott' _)) z in go

newtype HybridList a = ListSC { unconsSC :: forall r. (a -> ChurchList a -> r) -> r -> r }
  deriving (Functor)

instance ListLike HybridList where
  nil = ListSC \c n -> n
  cons x xs = ListSC \c n -> c x $ unconsSC xs conscl nilcl

tailHL :: HybridList a -> Maybe (ChurchList a)
tailHL xs = unconsSC xs (const Just) Nothing

tailHL' :: HybridList a -> (ChurchList a -> r) -> r -> r
tailHL' xs = unconsSC xs . const

tailHL'' :: HybridList a -> MaybeCL' (ChurchList a)
tailHL'' xs = flip $ unconsSC xs . const

churchToHybrid :: ChurchList a -> HybridList a
churchToHybrid = convert

tailViaHybrid :: ChurchList a -> Maybe (ChurchList a)
tailViaHybrid = tailHL . churchToHybrid
-- churchToHybrid = fmap snd . foldr (\x -> justCL . (,) x . maybeCL nilcl (\(x', xs') -> conscl x' xs')) nothingCL

-- fmap (take 10 . clToList ) $ tailViaHybrid $ listToCL $ [1..11] ++ undefined
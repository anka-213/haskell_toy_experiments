{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
import Data.Tree
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.Bifunctor (Bifunctor(bimap))
import Control.Monad.State (State, MonadState (state), evalState)
import Control.Applicative (Const(..), Applicative (liftA2), ZipList (..))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Traversable (foldMapDefault)
import GHC.Exts (build, augment)
import Control.Monad.Identity (Identity(..))

nd :: [Tree ()] -> Tree ()
nd = Node ()

lf :: Tree ()
lf = nd []

-- >>> calcDepth $ nd [lf , nd [lf]]
-- 2
calcDepth :: Tree a -> Int
calcDepth (Node x []) = 0
calcDepth (Node x xs) = succ . maximum $ map calcDepth xs

-- getDeepest :: Tree a -> (Int, Tree a)
-- getDeepest (Node x []) = (1, Node x [])
-- getDeepest (Node x (y:ys)) = go 0 (Node x ys) y
-- -- getDeepest (Node x xs) = maximumBy (comparing fst) $ go (Node x []) <$> xs
--   where
--     go :: Int -> Tree a -> Tree a -> (Int, Tree a)
--     go n parent (Node a []) = (succ n , Node a [parent])
--     go n parent (Node a (t : ts)) = go n (Node a $ parent : ts) t

newtype ShowString = SStr String

instance Show ShowString where
    show (SStr s) = s

-- >>> fmap (SStr . drawTree . fmap show) $ getDeepest $ Node 1 [Node 2 [], Node 3 [Node 4 []] , Node 5 []]
-- (2,
-- 4
-- |
-- `- 3
--    |
--    `- 1
--       |
--       +- 2
--       |
--       `- 5
-- )
getDeepest :: Tree a -> (Int, Tree a)
getDeepest = go 0 []
  where
    go :: Int -> Forest a -> Tree a -> (Int, Tree a)
    go !n parents (Node a []) = (n , Node a parents)
    go !n parents (Node a (t : ts)) = maximumBy (comparing fst)
        [ go (succ n) [Node a $ parents ++ ts] t
        , go n (t:parents) (Node a ts) ]

-- >>> getPathToDeepest $ Node 1 [Node 2 [], Node 3 [Node 4 []] , Node 5 []]
-- (3,[1,3,4])
getPathToDeepest :: Tree a -> (Int, [a])
getPathToDeepest (Node a []) = (1, [a])
getPathToDeepest (Node a (tr : trs)) = maximumBy (comparing fst)
        [ bimap succ (a :) $ getPathToDeepest tr
        , getPathToDeepest (Node a trs) ]

-- >>> getDiameter $ Node 1 [Node 2 [], Node 3 [Node 4 []] , Node 5 []]
-- (4,[4,3,1,5])
getDiameter :: Tree a -> (Int, [a])
getDiameter = getPathToDeepest . snd . getDeepest

type Numbering a = State Int a

setNumbers :: a -> Numbering Int
setNumbers _ = state $ \n -> (n , succ n)

dfNumbering :: Tree a -> Tree Int
dfNumbering t = evalState (traverse setNumbers t) 0

example :: Tree Int
example = dfNumbering $ nd [nd [lf,lf], nd [nd [nd[lf]],lf]]

-- >>> SStr $ drawTree $ fmap show $ example
-- 0
-- |
-- +- 1
-- |  |
-- |  +- 2
-- |  |
-- |  `- 3
-- |
-- `- 4
--    |
--    +- 5
--    |  |
--    |  `- 6
--    |     |
--    |     `- 7
--    |
--    `- 8

-- >>> getDiameter $ example
-- (7,[7,6,5,4,0,1,3])

-- >>> bfTraverse collect $ Node 1 [Node 2 [], Node 3 [Node 4 []] , Node 5 []]
bfTraverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
bfTraverse f (Node x xs) = Node <$> f x <*> bfTraverseForest f xs
bfTraverseForest :: Applicative f => (a -> f b) -> [Tree a] -> f [Tree b]
bfTraverseForest f [] = pure []
bfTraverseForest f (Node a children : neigh) =
    (\x ([chld],neigh) -> Node x chld : neigh) <$> f a <*> go f [children] neigh
  where
    go :: Applicative f => (a -> f b) -> [[Tree a]] -> [Tree a] -> f ([[Tree b]], [Tree b])
    go f todo [] = (,[]) <$> traverse (bfTraverseForest f) todo
    go f todo (Node a children : neigh) =
        (\x (chld : rest,neigh) -> (rest , Node x chld : neigh)) <$> f a <*> go f (children : todo) neigh

-- >>> bfNumbering $ nd [nd [lf,lf], nd [nd [nd[lf]],lf]]
-- Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = [Node {rootLabel = 7, subForest = []},Node {rootLabel = 8, subForest = []}]},Node {rootLabel = 2, subForest = [Node {rootLabel = 3, subForest = [Node {rootLabel = 5, subForest = [Node {rootLabel = 6, subForest = []}]}]},Node {rootLabel = 4, subForest = []}]}]}
bfNumbering :: Tree a -> Tree Int
bfNumbering t = evalState (bfTraverse setNumbers t) 0

-- putStrLn . drawTree . fmap show $ bfNumbering' $ nd [nd [nd[lf],nd[lf]], nd [nd [nd[lf]],lf],lf]

data (f :*: g) a = f a :*: g a
  deriving (Functor, Foldable, Traversable)

bfNumbering' :: Tree a -> Tree Int
bfNumbering' t = evalState (bfTraverse' setNumbers t) 0

bfTraverse' :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
bfTraverse' f (Node x xs) = Node <$> f x <*> bfTraverseForest' f xs

newtype BFForest a = BFF {unBFF :: Forest a}
  deriving (Functor)

instance Foldable BFForest where
  foldMap = foldMapDefault
--   foldr = _

instance Traversable BFForest where
  traverse f (BFF t) = BFF <$> bfTraverseForest' f t

bfTraverseForest' :: Applicative f => (a -> f b) -> [Tree a] -> f [Tree b]
bfTraverseForest' f = fmap snd . go f (Const ())
  where
    go :: (Applicative f, Traversable z) => (a -> f b) -> z a -> [Tree a] -> f (z b, [Tree b])
    go f todo [] = (,[]) <$> traverse f todo
    go f todo (Node a children : neigh) =
        (\x (done :*: BFF chld,neigh) -> (done , Node x chld : neigh)) <$> f a <*> go f (todo :*: BFF children) neigh



-- bfTraverseForestM f [] = pure []
-- bfTraverseForestM f (Node a children : neigh) = do
--     b <- f a
--     bs <- bfTraverseForestM f neigh
--     (\x (chld,neigh) -> Node x chld : neigh) <$> f a <*> go f children neigh
    -- go :: Applicative f => (a -> f b) -> [Tree a] -> [Tree a] -> f ([Tree b], [Tree b])
    -- go f todo [] = _ <$> bfTraverseForest f todo
    -- go f todo (Node a children : neigh) = _ $
    --     (\x (chld,neigh) -> (_ , Node x chld : neigh)) <$> f a <*> go f children neigh

newtype BFTree a = BFT {getBFT :: Tree a}
  deriving Functor


bftToList :: BFTree a -> [a]
bftToList (BFT t) =
    map rootLabel $
    concat $
    takeWhile (not . null) $
    iterate (concatMap subForest) [t]

bftToList'' :: BFTree a -> [a]
bftToList'' (BFT t) = go [t]
  where
    go [] = []
    go xs = fmap rootLabel xs ++ go (concatMap subForest xs)
    -- go xs = fmap rootLabel xs ++ go1 (map subForest xs)
    -- go1 [] = [] -- TODO: wrong test! Need to check if concat xs == []
    -- -- go1 xs = fmap rootLabel (concat xs) ++ go1 (map subForest $ concat xs)
    -- -- go1 xs = concatMap (map rootLabel) xs ++ go1 (concatMap (map subForest) xs)
    -- -- go1 xs = (concatMap . map) rootLabel xs ++ go1 ((concatMap . map) subForest xs)
    -- -- go1 xs = (concatMap . map) rootLabel xs ++ go2 ((map . map) subForest xs)
    -- -- go1 xs = foldr ((++) . map rootLabel) [] xs ++ go2 ((map . map) subForest xs)
    -- -- go1 xs =  build (\c n -> foldr (flip (foldr (c . rootLabel))) n xs) ++ go2 ((map . map) subForest xs)
    -- -- go1 xs' = augment (\c n -> foldr (flip (foldr (c . rootLabel))) n xs') (go2 ((map . map) subForest xs'))
    -- go1 xs' = foldr (flip (foldr ((:) . rootLabel))) (go2 ((map . map) subForest xs')) xs'

    -- go2 [] = []
    -- -- go2 xs = map rootLabel (concat $ concat xs) ++ go2 ((map . map) subForest $ concat xs)
    -- -- go2 xs = (concatMap . map) rootLabel (concat xs) ++ go2 ((map . map) subForest $ concat xs)
    -- go2 xs = (concatMap . concatMap . map) rootLabel xs ++ go2 ((concatMap . map . map) subForest xs)

bftToList''' :: BFTree a -> [a]
bftToList''' (BFT t) = concat $ go [t]
  where
    go :: [Tree a] -> [[a]]
    go [] = []
    -- go xs = fmap rootLabel xs : go (concatMap subForest xs)
    go xs = fmap rootLabel xs : foldr (composeSideways . (go . subForest)) [] xs

{-# ANN bftFoldr' "HLint: ignore Avoid lambda" #-}
bftFoldr' :: (a -> b -> b) -> b -> BFTree a -> b
-- bftFoldr' c n (BFT t) = go [t] (\x y -> foldr c y x) n
bftFoldr' c n (BFT t) = go0 [t] c n
  where
    go0 :: [Tree a] -> (a -> b -> b) -> b -> b
    go0 [] c n = n
    -- go0 xs c n = foldr c (go0 (concatMap subForest xs) c n) (fmap rootLabel xs)
    go0 xs c0 n0 = flip (foldr c0) (fmap rootLabel xs)
                 $ foldr ($) n0
                 $ foldr (\a bs -> build $ \c n -> go (subForest a)
                                                      (foldr2_left (zipWithFullFB (appendFB $ (.) . c0) c) (c . c'))
                                                      -- (foldr2_left (zipWithFullFB (flip $ foldr $ (.) . c0) c) (c . c'))
                                                      -- (foldr2_left (\as b0 -> c (flip (foldr ((.) . c0)) as b0)) (c . c'))
                                                      (foldr c n) bs
                         ) [] xs
      where -- c' = \x y -> foldr c0 y x
            appendFB c x y = foldr c y x
            c' = appendFB c0
    -- go0 xs c0 n = flip (foldr c0) (fmap rootLabel xs) $ foldr c0 n $ foldr (\a bs -> go (subForest a) (foldr2_left (\a b -> _ a b) _) id bs) [] xs
    -- go0 xs c0 n = flip (foldr c0) (fmap rootLabel xs) $ foldr c' n $ foldr (\a bs -> build $ \c n -> go (subForest a) (foldr2_left (\a b -> c (a ++ b)) c) (foldr c n) bs) [] xs
    --   where c' = \x y -> foldr c0 y x
    -- go0 xs c0 n = c' (fmap rootLabel xs) $ foldr c' n $ foldr (\a bs -> build $ \c n -> go (subForest a) (foldr2_left (\a b -> c (a ++ b)) c) (foldr c n) bs) [] xs

    -- go :: [Tree a] -> [[a]]
    go :: [Tree a] -> ([a] -> b -> b) -> b -> b
    go [] c n = n
    -- go xs c n = fmap rootLabel xs `c` go (concatMap subForest xs) c n
    -- go xs = fmap rootLabel xs : foldr (composeSideways . (go . subForest)) [] xs
    -- go xs c n = fmap rootLabel xs `c` foldr (\a b -> foldr c n $ composeSideways (go (subForest a) (:) []) (_ b)) n xs
    -- go xs c n = c (fmap rootLabel xs) $ foldr c n $ foldr (\a -> composeSideways (build $ go (subForest a))) [] xs
    -- go xs c n = c (fmap rootLabel xs) $ foldr c n $ foldr (\a bs -> build (\c n -> foldr2 (composeSidewaysFB c) c c n (build $ go (subForest a)) bs)) [] xs
    -- go xs c n = c (fmap rootLabel xs) $ foldr c n $ foldr (\a bs -> build $ \c n -> go (subForest a) (foldr2_left (composeSidewaysFB c) c) (foldr c n) bs) [] xs
    go xs c n = c (fmap rootLabel xs) $ foldr c n $ foldr (\a bs -> build $ \c n -> go (subForest a) (foldr2_left (\a b -> c (a ++ b)) c) (foldr c n) bs) [] xs

-- ℕ=∀𝑟.𝑟→(ℕ→𝑟)→𝑟
newtype ScottNat = SN (forall r. r -> (ScottNat -> r) -> r)
newtype ScottList a = SL (forall r. (a -> ScottList a -> r) -> r -> r)

foldSN :: b -> (b -> b) -> ScottNat -> b
foldSN z s = go
  where
    go (SN n) = n z (s . go)

foldSL :: (a -> b -> b) -> b -> ScottList a -> b
-- foldSL f x0 (SL xs) = xs (\x -> f x . foldSL f x0) x0
foldSL f z = go
  where
    go (SL xs) = xs (\x -> f x . go) z

consSL :: a -> ScottList a -> ScottList a
consSL x sl = SL $ \c _ -> c x sl

nilSL :: ScottList a
nilSL = SL $ \_ n -> n

foldr2SL :: (a -> b -> c -> c) -> (a -> c -> c) -> (b -> c -> c)  -> c -> ScottList a ->  ScottList b -> c
foldr2SL k l r z = go
  where
        go (SL xs) (SL ys) = xs
          (\x xs' -> ys
              (\y ys' -> k x y $ go xs' ys') -- go (consSL x xs') (consSL y ys')
              (x `l` foldSL l z xs'))        -- go (consSL x xs') nilSL
          (foldSL r z (SL ys))               -- go nilSL ys
        -- go []    ys      = foldr r z ys
        -- go xs    []      = foldr l z xs
        -- go (x:xs) (y:ys) = k x y (go xs ys)

-- Idea: Use stream fusion instead of list fusion

-- >>> composeSideways [[1,2], [3]] [[4],[5,6],[7]]
-- [[1,2,4],[3,5,6],[7]]
composeSideways :: Semigroup m => [m] -> [m] -> [m]
composeSideways [] bs = bs
composeSideways as [] = as
composeSideways (a : as) (b : bs) = a <> b : composeSideways as bs
-- composeSideways = zipWithDefault (<>) id id

composeSideways' :: Semigroup m => [m] -> [m] -> [m]
composeSideways' as bs = build (\c n -> foldr2 (composeSidewaysFB c) c c n as bs)

composeSidewaysFB :: Semigroup m => (m -> b -> b) -> m -> m -> b -> b
composeSidewaysFB k a b = k (a <> b)

zipWithFullFB :: (a -> b -> c) -> (c -> r -> r) -> a -> b -> r -> r
zipWithFullFB f k a b = k (f a b)


foldr2 :: (a -> b -> c -> c) -> (a -> c -> c) -> (b -> c -> c)  -> c -> [a] -> [b] -> c
foldr2 k l r z = go
  where
        go []    ys      = foldr r z ys
        go xs    []      = foldr l z xs
        -- go (x:xs) []     = x `l` go xs []
        go (x:xs) (y:ys) = k x y (go xs ys)
{-# INLINE [0] foldr2 #-}  -- See Note [Fusion for foldrN]

foldr2_left :: (a -> b -> c -> d) -> (a -> c -> d) -> a -> ([b] -> c) -> [b] -> d
foldr2_left _k l x r []     = l x $ r []
foldr2_left  k l x r (y:ys) = k x y (r ys)

foldr2' :: (a -> b -> c -> c) -> (a -> c -> c) -> (b -> c -> c)  -> c -> [a] -> [b] -> c
foldr2' k l r z = foldr (foldr2_left k l) (foldr r z)

-- foldr2 k z xs ys = foldr (foldr2_left k z)  (\_ -> z) xs ys
{-# RULES   -- See Note [Fusion for foldrN]
"foldr2/left"   forall k l r z ys (g::forall b.(a->b->b)->b->b) .
                  foldr2 k l r z (build g) ys = g (foldr2_left k l) (foldr r z) ys
 #-}


zipWithDefault :: (a -> b -> c) -> (a -> c) -> (b -> c) -> [a] -> [b] -> [c]
zipWithDefault comb a0 b0 (a : as) (b : bs) = comb a b : zipWithDefault comb a0 b0 as bs
zipWithDefault comb a0 b0 [] [] = []
zipWithDefault comb a0 b0 [] (b : bs) = b0 b : fmap b0 bs
zipWithDefault comb a0 b0 (a : as) [] = a0 a : fmap a0 as

bftToList' :: BFTree a -> [a]
bftToList' = go0 . getBFT
  where
    go [] = []
    -- go (Node r sf:xs) = r : fmap rootLabel xs ++ go (sf ++ concatMap subForest xs)
    go xs = fmap rootLabel xs ++ go (concatMap subForest xs)

    go0 t = rootLabel t : go (subForest t)

bftFoldr0 :: (a -> b -> b) -> b -> BFTree a -> b
bftFoldr0 (-:) x0 = go0 . getBFT
  where
    -- go [] = x0
    -- -- go xs = foldr ((-:) . rootLabel) (go $ map subForest $ concat xs) $ concat xs
    -- go xs = foldr (flip (foldr ((-:) . rootLabel))) (go $ concatMap (map subForest) xs) xs
    -- go0 t = rootLabel t -: go [subForest t]

    go [] = x0
    go (Node r sf:xs) = r -: foldr (-:) (go $ sf ++ concatMap subForest xs) (fmap rootLabel xs)
    -- go (Node r sf:xs) = r -: foldr (-:) (go $ sf ++ concatMap subForest xs) (fmap rootLabel xs)
    -- go [] = x0
    -- go xs = foldr (-:) (go $ build (\c n -> foldr (\x b -> foldr c b (subForest x)) n xs)) $ fmap rootLabel xs
    -- go xs = foldr (-:) (go $ concatMap subForest xs) $ fmap rootLabel xs
    -- go xs = foldr ((-:) . rootLabel) (go $ concatMap subForest xs) xs

    go0 t = rootLabel t -: go (subForest t)

    -- go0 y = rootLabel y -: go (subForest y ++ [])

{-
concatMap               :: (a -> [b]) -> [a] -> [b]
concatMap f             =  foldr ((++) . f) []

{-# NOINLINE [1] concatMap #-}

{-# RULES
"concatMap" forall f xs . concatMap f xs =
    build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)
 #-}
-}
    -- go . (:[]) . getBFT

    -- go y = if not (null y)
    --     then foldr ((-:) . rootLabel) (go (concatMap subForest y)) y
    --     else x0
    -- go y = if not (null y)
    --     then foldr (f . rootLabel) (go (concatMap subForest y)) y
    --     else x0
    -- go y =  if not (null y) then foldr (f . rootLabel) (go (concatMap subForest y)) y else x0
    -- go y = (\x r -> if not (null x) then foldr (f . rootLabel) r x else x0) y $ go (concatMap subForest y)

    -- foldr (\x r -> if not (null x) then foldr (f . rootLabel) r x else x0) x0 .
    -- iterate (concatMap subForest) .

    -- foldr (\x y -> foldr (f . rootLabel) y x) x0 .
    -- takeWhile (not . null) .

-- bftFoldr f x0 =
--     foldr (f . rootLabel) x0 .
--     concat .
--     takeWhile (not . null) .
--     iterate (concatMap subForest) .
--     (:[]) .
--     getBFT

bffFoldr :: (a -> b -> b) -> b -> BFForest a -> b
bffFoldr k z = go . unBFF
  where
    go [] = z
    go xs = foldr (k . rootLabel) (go $ concatMap subForest xs) xs

bftFoldr :: (a -> b -> b) -> b -> BFTree a -> b
bftFoldr k z = bffFoldr k z . BFF . (:[]) . getBFT
-- bftFoldr k z = go . (:[]) . getBFT
--   where
--     go [] = z
--     go xs = foldr (k . rootLabel) (go $ concatMap subForest xs) xs

bftFoldMap :: Monoid m => (a -> m) -> BFTree a -> m
bftFoldMap f = go . (:[]) . getBFT
  where
    go [] = mempty
    go xs = foldMap (f . rootLabel) xs <> go (concatMap subForest xs)

instance Foldable BFTree where
  foldMap = bftFoldMap
  foldr f x0 = foldr f x0 . bftToList

instance Traversable BFTree where
  traverse = bftTraverse
  sequenceA = bftSequenceA

mkBFT :: a -> [Tree a] -> BFTree a
mkBFT x t = BFT $ Node x t

bftTraverse :: Applicative f => (a -> f b) -> BFTree a -> f (BFTree b)
-- bftTraverse k (BFT t) = mkBFT <$> k (rootLabel t) <*> go [t]
bftTraverse k (BFT (Node t0 ts0)) = mkBFT <$> k t0 <*> go ts0
  where
    go [] = pure []
    -- go xs = foldMap (f . rootLabel) xs <> go (concatMap subForest xs)
    go ts = zipWith Node <$> traverse k rs <*> fmap (rebuild css) (go $ concat css)
        where
          (rs, css) = unzip $ map (\(Node r cs) -> (r, cs)) ts
          -- rebuild s d = evalState (traverse (state splitAt') d) s
          -- I think, but let's keep the dependencies down, shall we?
          rebuild [] [] = []
          rebuild (struct : structs) destruct
            = let (cs, destruct') = splitAt' struct destruct
              in  cs : rebuild structs destruct'
          rebuild [] (x:xs) = error "impossible"

          -- ignoring the as in a [a] makes it look like a number
          splitAt' [] xs = ([], xs)
          splitAt' (_ : n) (x : xs)
            = let (pre, suf) = splitAt' n xs
              in  (x : pre, suf)
          splitAt' (_ : n) [] = error "impossible"

bftTraverse1 :: forall f a b. Applicative f => (a -> f b) -> BFTree a -> f (BFTree b)
-- bftTraverse1 k (BFT t) = mkBFT <$> k (rootLabel t) <*> go [t]
bftTraverse1 k (BFT (Node t0 ts0)) = mkBFT <$> k t0 <*> go ts0
-- bftTraverse1 k (BFT t) = BFT . runIdentity <$> go (Identity t)
  where
    -- go [] = pure []
    -- go xs = foldMap (f . rootLabel) xs <> go (concatMap subForest xs)
    nodeC x xs = Node x (getCompose xs)
    go :: (Zippable t, Traversable t) => t (Tree a) -> f (t (Tree b))
    -- go ts | null ts = _
    go ts | Just ts' <- nullMap ts = pure ts'
    go ts = myZipWith Node <$> traverse k rs <*> fmap getCompose (go $ Compose css)
    -- go ts = myZipWith nodeC <$> traverse k rs <*> fmap (fmap Compose . getCompose) (go $ Compose css)
        where
          rs = fmap rootLabel ts
          css = fmap subForest ts

bftTraverse2 :: forall f a b. Applicative f => (a -> f b) -> BFTree a -> f (BFTree b)
bftTraverse2 k (BFT (Node t0 ts0)) = mkBFT <$> k t0 <*> fmap getZipList (go (ZipList ts0))
  where
    nodeC x xs = Node x (getZipList xs)
    -- go :: (Zippable t, Traversable t) => t (Tree a) -> f (t (Tree b))
    -- go ts = myZipWith Node <$> traverse (k . rootLabel) ts <*> fmap getCompose (go $ Compose css)
    go :: (Applicative t, Traversable t) => t (Tree a) -> f (t (Tree b))
    go ts | Just ts' <- nullMap ts = pure ts'
    go ts = liftA2 nodeC <$> traverse (k . rootLabel) ts <*> fmap getCompose (go $ Compose css)
        where
          css = fmap (ZipList . subForest) ts

nullMap :: Traversable t => t a -> Maybe (t b)
nullMap = traverse (const Nothing)

class Traversable t => Zippable t where
    myZipWith :: (a -> b -> c) -> t a -> t b -> t c
    -- nullMap :: t a -> Maybe (t b)

instance Zippable [] where
  myZipWith = zipWith
--   nullMap [] = Just []
--   nullMap _ = Nothing

instance (Zippable f, Zippable g) => Zippable (Compose f g) where
  myZipWith f (Compose a) (Compose b) = Compose $ myZipWith (myZipWith f) a b
--   nullMap (Compose a) = Compose <$> traverse nullMap a


-- myZipWith :: (a0 -> t1 a0 -> Tree a0) -> t b -> t (t1 b) -> t (Tree b)
-- myZipWith :: (a0 -> Compose [] Tree a0 -> Tree a0) -> t b -> Compose t [] (Tree b) -> t (Tree b)

-- myZipWith :: (a -> [Tree a] -> Tree a) -> t b -> t [Tree b] -> t (Tree b)
-- myZipWith = undefined


bftSequenceA :: Applicative f => BFTree (f a) -> f (BFTree a)
bftSequenceA = bftTraverse id

bfNumbering3 :: Tree a -> Tree Int
bfNumbering3 t = getBFT $ evalState (bftTraverse setNumbers (BFT t)) 0

bfNumbering4 :: Tree a -> Tree Int
bfNumbering4 t = getBFT $ evalState (bftTraverse1 setNumbers (BFT t)) 0
bfNumbering5 :: Tree a -> Tree Int
bfNumbering5 t = getBFT $ evalState (bftTraverse2 setNumbers (BFT t)) 0

-- putStrLn . drawTree . fmap show $ bfNumbering5 $ nd [nd [nd[lf],nd[lf]], nd [nd [nd[lf]],lf],lf]

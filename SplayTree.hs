{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module SplayTree where
import Data.Foldable (toList)
import GHC.Generics (Generic)
import qualified Data.Tree as Tree

data SplayTree a = Empty | Node  (SplayTree a) a (SplayTree a)
   deriving (Show, Eq)
   deriving (Functor, Foldable)
   deriving (Generic)


{-

When a node x is accessed, a splay operation is performed on x to move it to the root. 
To perform a splay operation we carry out a sequence of splay steps, each of which moves x closer to the root.
By performing a splay operation on the node of interest after every access,
the recently accessed nodes are kept near the root and the tree remains roughly balanced, so that we achieve the desired amortized time bounds.

Each particular step depends on three factors:

- Whether x is the left or right child of its parent node, p,
- whether p is the root or not, and if not
- whether p is the left or right child of its parent, g (the grandparent of x).

It is important to remember to set gg (the great-grandparent of x) to now point to x after any splay operation. 
If gg is null, then x obviously is now the root and must be updated as such.

There are three types of splay steps, each of which has two symmetric variants: left- and right-handed. 
For the sake of brevity, only one of these two is shown for each type. (In the following diagrams, circles 
indicate nodes of interest and triangles indicate sub-trees of arbitrary size.) The three types of splay steps are:


-}


{- | Zig step: this step is done when p is the root. 

The tree is rotated on the edge between x and p. Zig steps exist to deal with the parity issue, will be done only as the last step in a splay operation, and only when x has odd depth at the beginning of the operation.
-}

zigStepR :: SplayTree a -> SplayTree a
zigStepR (Node (Node a x b) p c) = Node a x (Node b p c)
zigStepR t = error $ "zigStepR: tree too shallow: " ++ show (() <$ t)

{- | Zig-zig step: this step is done when p is not the root and x and p are either both right children or are both left children.

The picture below shows the case where x and p are both left children. The tree is rotated on the edge joining p with its parent g,
then rotated on the edge joining x with p. Note that zig-zig steps are the only thing that differentiate splay trees from the rotate
 to root method introduced by Allen and Munro[5] prior to the introduction of splay trees.
-}
zigZigStepR :: SplayTree a -> SplayTree a
zigZigStepR (Node (Node (Node a x b) p c) g d) = Node a x (Node b p (Node c g d))
zigZigStepR t = error $ "zigZigStepR: tree too shallow: " ++ show (() <$ t)


{- | Zig-zag step: this step is done when p is not the root and x is a right child and p is a left child or vice versa (x is left, p is right). 

The tree is rotated on the edge between p and x, and then rotated on the resulting edge between x and g.
-}
zigZagStepR :: SplayTree a -> SplayTree a
zigZagStepR (Node (Node a x (Node b y c)) z d) = Node a x (Node (Node b y c) z d)
zigZagStepR t = error $ "zigZagStepR: tree too shallow: " ++ show (() <$ t)

-- | The derivative of the SplayTree type. A SplayTree with a single `a` missing.
-- T x = 1 + T*x*T
-- T' x = T'*x*T + T*T + T*x*T'
data TreeZipperNaive a = Here' (SplayTree a) (SplayTree a) | MissingL' (TreeZipperNaive a) a (SplayTree a) | MissingR' (SplayTree a) a (TreeZipperNaive a)
   deriving (Show, Eq)

data TreeZipperNode a = MissingL a (SplayTree a) | MissingR (SplayTree a) a
   deriving (Show, Eq)
   deriving (Generic)

type TreeZipper a = [TreeZipperNode a]

{-

-- ghciWith QuickCheck generic-arbitrary

:l SplayTree.hs
:m + Test.QuickCheck Test.QuickCheck.Arbitrary.Generic

instance Arbitrary a => Arbitrary (SplayTree a) where arbitrary = genericArbitrary ; shrink = genericShrink
instance Arbitrary a => Arbitrary (TreeZipperNode a) where arbitrary = genericArbitrary ; shrink = genericShrink

-- quickCheck prop_splayZipper

xs <- sample' (arbitrary :: Gen (SplayTree Int))
ys <- sample' (arbitrary :: Gen (TreeZipper Int))

import qualified Data.Tree as Tree
toTree Empty = []; toTree (Node l x r) = [Tree.Node x (toTree l <> toTree r)]

putStrLn . Tree.drawForest . (fmap.fmap) show $ toTree $ unZipper (xs !! 2) (ys !! 1)

-}

toForest :: SplayTree a -> [Tree.Tree a]
toForest Empty = []; toForest (Node l x r) = [Tree.Node x (toForest l <> toForest r)]

toStrTree :: SplayTree String -> Tree.Tree String
toStrTree Empty = Tree.Node " " []
toStrTree (Node l x r) = Tree.Node x [toStrTree l , toStrTree r]

drawTree :: SplayTree String -> String
drawTree = Tree.drawTree . toStrTree
-- drawTree = Tree.drawForest . toForest

printTree :: Show a => SplayTree a -> IO ()
printTree = putStrLn . drawTree . fmap show

unZipper :: SplayTree a -> TreeZipper a -> SplayTree a
unZipper t [] = t
unZipper t (MissingL   x r : zs) = unZipper (Node t x r) zs
unZipper t (MissingR l x   : zs) = unZipper (Node l x t) zs

splayZipper :: SplayTree a -> TreeZipper a -> SplayTree a
splayZipper t [] = t
splayZipper Empty (MissingL   x b : zp) = splayZipper (Node Empty x b    ) zp
splayZipper Empty (MissingR a x   : zp) = splayZipper (Node a     x Empty) zp
splayZipper (Node a x b) [MissingL   y c] = Node a x (Node b y c)              -- Zig step
splayZipper (Node b y c) [MissingR a x  ] = Node (Node a x b) y c              -- Zig step
splayZipper (Node a x b) (MissingL   y c : MissingL   z d : zp)                -- Zig-zig step
  = splayZipper (Node a x (Node b y (Node c z d))) zp
splayZipper (Node c x d) (MissingR b p   : MissingR a g   : zp)                -- Zig-zig step
  = splayZipper (Node (Node (Node a g b) p c) x d) zp
splayZipper (Node b x c) (MissingL   p d : MissingR a gp  : zp)                -- Zig-zag step
  = splayZipper (Node (Node a gp b) x (Node c p d)) zp
splayZipper (Node b x c) (MissingR a p   : MissingL   g d : zp)                -- Zig-zag step
  = splayZipper (Node (Node a p b) x (Node c g d)) zp

-- splayZipper :: a -> TreeZipper a -> SplayTree a
-- splayZipper = _

-- 

-- | splayZipper preserves the order of the tree.
prop_splayZipper :: SplayTree Int -> TreeZipper Int -> Bool
prop_splayZipper t z = toList (unZipper t z) == toList (splayZipper t z)

-- | Top-down splaying without intermediate structure
directSplay :: Ord a => a -> SplayTree a -> SplayTree a
-- directSplay x Empty = Node Empty x Empty
directSplay x Empty = Empty
directSplay x (Node l y r) = case compare x y of
  LT -> case l of
    Empty -> Node Empty y r  -- Elem is missing (might want to handle insertion here)
    Node ll ly lr -> case compare x ly of
      LT -> case directSplay x ll of
        Node lll lly llr -> Node lll lly (Node llr ly (Node lr y r)) -- Zig-zig
        ll'@Empty -> Node ll' ly (Node lr y r) -- Elem is missing
      EQ ->      Node ll ly (Node lr y r) -- Zig
      GT -> case directSplay x lr of
        Node lrl lry lrr -> Node (Node ll ly lrl) lry (Node lrr y r) -- Zig-zag
        lr'@Empty -> Node ll ly (Node lr' y r) -- Elem is missing
  EQ -> Node l y r  -- Already there
  GT -> case r of
    Empty -> Node l y Empty  -- Elem is missing
    Node rl ry rr -> case compare x ry of
      LT -> case directSplay x rl of
        Node rll rly rlr -> Node (Node l y rll) rly (Node rlr ry rr) -- Zig-zag
        rl'@Empty -> Node (Node l y rl') ry rr -- Elem is missing
      EQ ->      Node (Node l y rl) ry rr -- Zig
      GT -> case directSplay x rr of
        Node rrl rry rrr -> Node (Node (Node l y rl) ry rrl) rry rrr -- Zig-zig
        rr'@Empty -> Node (Node l y rl) ry rr' -- Elem is missing



goToZ :: Ord a => a -> SplayTree a -> (SplayTree a, TreeZipper a)
goToZ x t = goToZ' t []
  where
    -- goToZ' :: Ord a => SplayTree a -> TreeZipper a -> (SplayTree a, TreeZipper a)
    goToZ' Empty z = (Empty, z)
    goToZ' (Node l a r) z = case compare x a of
      LT -> goToZ' l (MissingL   a r : z)
      EQ -> (Node l a r, z)
      GT -> goToZ' r (MissingR l a   : z)

goTo :: Ord a => a -> SplayTree a -> SplayTree a
goTo x t = uncurry splayZipper $ goToZ x t

locate :: Ord a => a -> SplayTree a -> (SplayTree a, Maybe a)
locate x t = case goToZ x t of
   (Empty          , zp) -> (splayZipper Empty zp, Nothing)
   (t'@(Node _ a _), zp) -> (splayZipper t' zp, Just a)

insert :: Ord a => a -> SplayTree a -> SplayTree a
insert x t = case goToZ x t of
   (Empty          , zp) -> splayZipper (Node Empty x Empty) zp
   (Node l _ r     , zp) -> splayZipper (Node l x r) zp

delete :: Ord a => a -> SplayTree a -> SplayTree a
delete x t = case goTo x t of
   (Node l a r     ) | a == x -> join l r
   t'             -> t'

join :: SplayTree a -> SplayTree a -> SplayTree a
join a b = case goToLargestZ a of
    (Empty, zp) -> splayZipper b zp
    (nd, _) -> error $ "join: unexpected node: " ++ show (() <$ nd)


fromList :: Ord a => [a] -> SplayTree a
fromList = foldr insert Empty

goToLargestZ :: SplayTree a -> (SplayTree a, TreeZipper a)
goToLargestZ t = goToLargestZ' t []
  where
    -- goToLargestZ' :: Ord a => SplayTree a -> TreeZipper a -> (SplayTree a, TreeZipper a)
    goToLargestZ' Empty z = (Empty, z)
    goToLargestZ' (Node l a r) z = goToLargestZ' r (MissingR l a   : z)

goToLargest :: Ord a => SplayTree a -> SplayTree a
goToLargest t = uncurry splayZipper $ goToLargestZ t

-- | Move the node to the root of the tree.
splay :: Ord a => a -> SplayTree a -> SplayTree a
splay _ Empty = Empty
splay x t@(Node l y r)
  | x < y     = case splay x l of
                  Empty -> t
                  l'    -> Node l' y r
  | x > y     = case splay x r of
                  Empty -> t
                  r'    -> Node l y r'
  | otherwise = t
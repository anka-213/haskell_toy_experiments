{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BreadthFirstTraverse where
import Data.Tree (Tree(..))
import Control.Applicative (ZipList(..), Applicative (liftA2))
import Data.Functor.Compose (Compose(..))
-- import Control.Monad.Identity (Identity(..))

newtype Breadth a = Breadth (Tree a) deriving (Show)

breadth :: Tree a -> [a]
breadth tree = go [tree]
  where
    go [] = []
    go (Node x subs:q) = x : go (q <> subs)

instance Functor Breadth where
  fmap f (Breadth t) = Breadth $ fmap f t

instance Foldable Breadth where
  foldMap f (Breadth t) = mconcat $ f <$> breadth t

instance Traversable Breadth where
  traverse f (Breadth t) = Breadth <$> bfTraverse f t

bfTraverse :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
bfTraverse k (Node t0 ts0) = nodeC <$> k t0 <*> go (ZipList ts0)
-- bfTraverse k t = fmap runIdentity (go (Identity t))
  where
    nodeC x xs = Node x (getZipList xs)
    go :: (Applicative t, Traversable t) => t (Tree a) -> f (t (Tree b))
    go ts | Just ts' <- nullMap ts = pure ts'
    go ts = liftA2 nodeC <$> traverse k rs <*> fmap getCompose (go $ Compose css)
        where
          rs = fmap rootLabel ts
          css = fmap (ZipList . subForest) ts

-- | If a structure is empty, replace its content type
-- > isJust . nullMap == null
nullMap :: Traversable t => t a -> Maybe (t b)
nullMap = traverse (const Nothing)

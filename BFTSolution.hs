{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveFunctor #-} -- REMOVE
module BFTSolution where

import Control.Applicative (ZipList(..), Applicative (liftA2))
import Data.Functor.Compose (Compose(..))

import Data.Traversable (foldMapDefault)
import Control.Monad.Identity (Identity(..))
-- import TreeDef (Tree(..))

data Tree a = a :& [Tree a] deriving (Eq, Ord, Show, Functor)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse k t = fmap runIdentity (traverseForest k (Identity t))
--   traverse = bfTraverse

mkNode x xs = x :& getZipList xs
getRoot (x :& _) = x
getChildren (_ :& xs) = ZipList xs

traverseForest :: (Applicative f, Applicative t, Traversable t) => (a -> f b) -> t (Tree a) -> f (t (Tree b))
traverseForest k ts | Just ts' <- traverse (const Nothing) ts = pure ts'
traverseForest k ts = liftA2 mkNode <$> traverse k (getRoot<$>ts) <*> fmap getCompose (traverseForest k $ Compose $ getChildren <$> ts)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}

-- Miniature version of https://hackage.haskell.org/package/gdp
-- See https://kataskeue.com/gdp.pdf

module MiniGDP
  ( -- * Named values
    Named, type (~~)
  , name
  -- * The
  , The(..)
  , pattern The
  -- * Inequality
  , Smaller
  , compareNamed
  , NegInfty
  , Infinity
  , cmpNegInfty
  , cmpInfinity
  , cmpTrans
  ) where

import Data.Coerce

----------------
-- Inequality --
----------------

data Smaller a b = AssertSmaller

instance Show (Smaller a b) where show _ = "_"

compareNamed :: Ord a => Named la a -> Named lb a -> Either (Smaller la lb) (Smaller lb la)
compareNamed (The a) (The b) 
  | a < b     = Left AssertSmaller
  | otherwise = Right AssertSmaller

data Infinity
data NegInfty

cmpNegInfty :: Smaller NegInfty a
cmpNegInfty = AssertSmaller

cmpInfinity :: Smaller a Infinity
cmpInfinity = AssertSmaller

cmpTrans :: Smaller a b -> Smaller b c -> Smaller a c
cmpTrans AssertSmaller AssertSmaller = AssertSmaller

---------
-- The --
---------

class The d a | d -> a where
  the :: d -> a
  default the :: Coercible d a => d -> a
  the = coerce

{-| A view pattern for discarding the wrapper around
    a value.
-}
pattern The :: The d a => a -> d
pattern The x <- (the -> x)

{--------------------------------------------------
  Named values
--------------------------------------------------}

-- | A value of type @a ~~ name@ has the same runtime
--   representation as a value of type @a@, with a
--   phantom "name" attached.
newtype Named name a = Named a
  deriving newtype Show
type role Named nominal nominal

-- | An infix alias for 'Named'.
type a ~~ name = Named name a

instance The (Named name a) a

-- | Introduce a name for the argument, and pass the
--   named argument into the given function.
name :: a -> (forall name. a ~~ name -> t) -> t
name x k = k (coerce x)
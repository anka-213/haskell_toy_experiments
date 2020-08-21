{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module GenericToTuple where

import GHC.Generics -- (Generic)

data Quad' a b c d = Quad' a b c d
  deriving Generic

data X = X

data Quad = Quad X X X X
  deriving Generic

class ToTuple x where
    type TupleType x :: *
    toTuple :: x -> TupleType x

-- ggtoTuple :: (Generic x, Gtt (Rep x z)) => x -> Gtp (Rep x z)
-- ggtoTuple :: (Gtt (Rep a x), Generic a) => a -> Gtp (Rep a x)
ggtoTuple = gtoTuple . from

-- $> ggtoTuple 

-- hello :: Gtt (Rep Quad x) => Gtp (Rep Quad x)
hello = gtoTuple . from $ Quad X X X X
-- hello = ggtoTuple $ Quad X X X X

-- class Gtt x res | x -> res where
class Gtt x res where
    gtoTuple :: x -> X -- Gtp x

instance (Gtt (a p) res) => Gtt (M1 i meta a p) res where
    gtoTuple (M1 a) = gtoTuple a

-- instance (Gtt (a p) res1, Gtt (b p) res2, res3 ~ PlusTup res1 res2) => Gtt ((:*:) a b p) res3 where
instance (Gtt (a p) res1, Gtt (b p) res2, res3 ~ PlusTup res1 res2) => Gtt ((:*:) a b p) res3 where


type family PlusTup a b

-- class Gtt x where
--     type Gtp x :: *
--     gtoTuple :: x -> X -- Gtp x

-- instance Gtt (a p) => Gtt (M1 i meta a p) where
--     type Gtp (M1 i meta a p) = Gtp (a p)
--     gtoTuple (M1 a) = gtoTuple a

-- instance (Gtt (a p), Gtt (b p)) => Gtt ((:*:) a b p) where
--     type Gtp ((:*:) a b p) = PlusTup (Gtp (a p)) (Gtp (b p))


-- type family PlusTup a b

-- Inspired by these blog posts:
-- https://kavigupta.org/2016/05/08/Haskell-Classes-For-Products-And-Coproducts/
-- https://kavigupta.org/2016/05/10/A-Haskell-Class-For-Limits/
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

import Prelude hiding (fst, snd, id, (.))
-- import Prelude qualified
import Data.Void (Void, absurd)
import Data.Kind (Type, Constraint)
import Data.Type.Equality ( type (:~:)(Refl) )
import Control.Category ( Category(..) )
-- import Data.Typeable (TypeRep)
-- import GHC.Internal.TypeLits (KnownNat)
-- import Data.Data (Proxy(Proxy))
-- import Data.Functor.Const (Const)
-- import Data.Singletons


-- No laws
class Span s where
    fst :: s a b -> a
    snd :: s a b -> b

class Span' s a b | s -> a , s -> b where
    fst' :: s -> a
    snd' :: s -> b

instance Span' (a, b) a b where
    fst' (x, _) = x
    snd' (_, y) = y

instance Span (,) where
    fst (x, _) = x
    snd (_, y) = y

instance (Span ((,,) a)) where
    fst (_, x, _) = x
    snd (_, _, y) = y

class (Span s) => Product s where
    pFactor :: (Span s') => s' a b -> s a b


lawProduct :: forall s s' a b. (Eq a, Eq b, Span s', Product s) => s' a b -> Bool
lawProduct val' = fst val == fst val' && snd val == snd val'
    where
    val :: s a b
    val = pFactor val'

-- Additional law: Product should be unique (up to isomorphism)
-- productUnique :: forall s s' a b. (Eq a, Eq b, Span s', Product s) => (s' a b -> s a b) -> Bool

class (Span' s a b) => Product' s a b | s -> a , s -> b where
    pFactor' :: (Span' s' a b) => s' -> s

instance Product (,) where
    pFactor val' = (fst val', snd val')

instance Product ((,,) ()) where
    pFactor val' = ((), fst val', snd val')

-- Church encoding of pair
newtype CPSPair a b = P (forall r. (a -> b -> r) -> r)

instance Span CPSPair where
  fst (P p)= p const
  snd (P p)= p (const id)

instance Product CPSPair where
  pFactor spn = P $ \k -> k (fst spn) (snd spn)

mkProduct :: Product p => a -> b -> p a b
mkProduct a b = pFactor (a, b)

class Cospan s where
    left :: a -> s a b
    right :: b -> s a b

class Cospan' s a b | s -> a, s -> b where
    left' :: a -> s
    right' :: b -> s

instance Cospan Either where
    left = Left
    right = Right

instance Cospan' (Either a b) a b where
    left' = Left
    right' = Right

data TripleEither a b c = Ae a | Be b | Ce c
    deriving (Show, Eq)

instance Cospan (TripleEither a) where
    left = Be
    right = Ce

class (Cospan s) => Coproduct s where
    cpFactor :: (Cospan s') => s a b -> s' a b

-- This also needs to be unique
lawCoproduct :: forall s s' a b. (Eq (s' a b), Cospan s', Coproduct s) => a -> b -> Bool
lawCoproduct a b = cpFactor lhsA == rhsA && cpFactor lhsB == rhsB
    where
    rhsA, rhsB :: s' a b
    rhsA = left a
    rhsB = right b
    lhsA, lhsB :: s a b
    lhsA = left a
    lhsB = right b

instance Coproduct Either where
    cpFactor (Left a) = left a
    cpFactor (Right a) = right a

instance Coproduct (TripleEither Void) where
    cpFactor (Ae v) = absurd v
    cpFactor (Be b) = left b
    cpFactor (Ce c) = right c

newtype ChurchCoproduct a b = Coprod (forall r. (a -> r) -> (b -> r) -> r)

instance Cospan ChurchCoproduct where
  left a = Coprod $ \l _ -> l a
  right b = Coprod $ \_ r -> r b
--   left a = Coprod $ (const $) . ($ a)
--   right b = Coprod $ const ($ b)

instance Coproduct ChurchCoproduct where
  cpFactor (Coprod f) = f left right

-- Functor from subset of Hask to Hask
class ConstrainedFunctor (c :: Type -> Constraint) f where
    cfmap :: forall a b. (c a, c b) => (a -> b) -> f a -> f b

-- Diagrams are defined as functors from an index category to Hask.
-- j is the (set of objects of the) index category. Singletons of j are used to pattern match on objects.
-- Arrow in j are defined as a haskell type. I have not defined id and composition.
class Category (Arrow d) => Diagram (d :: j -> Type) where
    type Arrow d :: j -> j -> Type
    dmap :: forall (ja :: j) (jb :: j). Sing ja -> Sing jb -> Arrow d ja jb -> d ja -> d jb

-- instance Functor f => Diagram f where
--     type Arrow Type = (->)
--     dmap = fmap

class Diagram d => Cone n d | n -> d where
    indexCone :: Sing j -> n -> d j

coneLaw :: forall {j} {d :: j -> Type} {a :: j} {b :: j} {n} . (Eq (d b), Cone n d) => Sing a -> Sing b -> Arrow d a b -> n -> Bool
coneLaw a b f x = dmap a b f (indexCone @n @d a x) == indexCone b x

type family Sing :: k -> Type
-- type family Sing @k :: k -> Type

data SBool k where
    SFalse :: SBool False
    STrue :: SBool True

type instance Sing = SBool

type family DSpan' a b (sel :: Bool) :: Type where
    DSpan' a b False = a
    DSpan' a b True = b

newtype DSpan a b sel = DS { unDS :: DSpan' a b sel }

newtype Const2 k a b = Const2 k

instance Diagram (DSpan a b) where
    type Arrow (DSpan a b) = (:~:)
    dmap _ _ Refl = id


newtype DiscreteDiagram (fam :: a -> Type) (idx :: a) = DD { getDD :: fam idx }

instance Diagram (DiscreteDiagram fam) where
    type Arrow (DiscreteDiagram fam) = (:~:)
    dmap _ _ Refl = id
--     dmap (Const2 v) = absurd v

newtype SpanCone s a b = SpanCone { getSpanCon :: s a b }

instance Span s => Cone (SpanCone s a b) (DSpan a b) where
    indexCone :: Sing j -> SpanCone s a b -> DSpan a b j
    indexCone SFalse = DS . fst . getSpanCon
    indexCone STrue = DS . snd . getSpanCon

newtype Conespan c a b = Cs {unCs :: c a b}

instance (forall a b. Cone (c a b) (DSpan a b)) => Span (Conespan c) where
--   fst = unDS . indexCone SFalse . unCs
--   snd = unDS . indexCone STrue . unCs
  fst :: forall a b. Cone (c a b) (DSpan a b) => Conespan c a b -> a
  fst = unDS . indexCone @(c a b) @(DSpan a b) SFalse . unCs
  snd :: forall a b. Cone (c a b) (DSpan a b) => Conespan c a b -> b
  snd = unDS . indexCone @(c a b) @(DSpan a b) STrue . unCs

newtype Conespan' c a b = Cs' {unCs' :: c}
instance Cone c (DSpan a b) => Span' (Conespan' c a b) a b where
    fst' = unDS . indexCone SFalse . unCs'
    snd' = unDS . indexCone STrue . unCs'
--   fst :: forall a b. Cone (c a b) (DSpan a b) => Conespan c a b -> a
--   fst = unDS . indexCone @(c a b) @(DSpan a b) SFalse . unCs
--   snd :: forall a b. Cone (c a b) (DSpan a b) => Conespan c a b -> b
--   snd = unDS . indexCone @(c a b) @(DSpan a b) STrue . unCs

instance (forall a b. Limit (c a b) (DSpan a b)) => Product (Conespan c) where
    pFactor :: (Limit (c a b) (DSpan a b), Span s') => s' a b -> Conespan c a b
    pFactor = Cs . getLimit . SpanCone

-- instance (Limit c (DSpan a b)) => Product' (Conespan' c a b) a b where
--     pFactor' :: (Limit c (DSpan a b), Span' s' a b) => s' -> Conespan' c a b
--     pFactor' x = Cs' . pFactor' $ Cs (_ , _)

instance Product c => Limit (SpanCone c a b) (DSpan a b) where
    getLimit :: (Product c, Cone n' (DSpan a b)) => n' -> SpanCone c a b
    getLimit n' = SpanCone . pFactor $ (unDS $ indexCone SFalse n' , unDS $ indexCone STrue n')

-- instance Limit c (DSpan a b) => Product (Conespan' c) where

data SomeCone (d :: j -> Type) = forall c. Cone c d => MkSomeCone c
data SomeCone1 (d :: k -> j -> Type) (a :: k) = forall c. Cone (c a) (d a) => MkSomeCone1 (c a)
data SomeCone2 (d :: k -> k -> j -> Type) a b = forall c. Cone (c a b) (d a b) => MkSomeCone2 (c a b)

instance Span (SomeCone2 DSpan) where
    fst :: forall a b. SomeCone2 DSpan a b -> a
    fst (MkSomeCone2 x) = unDS $ indexCone @_ @(DSpan _ b)  SFalse x
    snd :: forall a b. SomeCone2 DSpan a b -> b
    snd (MkSomeCone2 x) = unDS $ indexCone @_ @(DSpan a _)  STrue x

-- Arbitrary size products



data HList (xs :: [Type]) where
    HNil :: HList '[]
    HCons :: a -> HList xs -> HList (a : xs)

data ListIndex (xs :: [Type]) where
    Here :: ListIndex (a : xs)
    There :: ListIndex xs -> ListIndex (a : xs)

type SListIndex :: ListIndex xs -> Type
data SListIndex (i :: ListIndex xs) :: Type where
    SHere :: SListIndex Here
    SThere :: SListIndex i -> SListIndex (There i)

type instance Sing = SListIndex

data Nat = Z | S Nat

data SNat (n :: Nat) where
    SZ :: SNat Z
    SS :: SNat n -> SNat (S n)

type instance Sing = SNat

data Fin (n :: Nat) where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin (S n)

type SFin :: Fin n -> Type
data SFin (f :: Fin n) :: Type where
    SFZ :: SFin FZ
    SFS :: SFin n -> SFin (FS n)

-- type SFin :: forall n -> Fin n -> Type
-- data SFin (n :: Nat) (f :: Fin n) :: Type where
--     SFZ :: SFin (S n) FZ
--     SFS :: SFin n f -> SFin (S n) (FS f)

-- class Forgetting n where
--     type Forget (f :: Fin n) :: Nat

-- instance Forgetting (S n) where
--     type Forget FZ = Z
--     type Forget (FS f) = S (Forget f)

-- type family Forget (f :: Fin n) :: Nat where
--     Forget FZ = Z

-- newtype SFin (f :: Fin n) = SFin (SNat (Forget f))

type instance Sing = SFin

type family Length (xs :: [Type]) where
    Length '[] = Z
    Length (x : xs) = S (Length xs)

type AtIndex' :: forall (xs :: [Type]) -> ListIndex xs -> Type
type family AtIndex' (xs :: [Type]) (j :: ListIndex xs) :: Type where
    AtIndex' (x : xs) Here = x
    AtIndex' (x : xs) (There n) = AtIndex' xs n

newtype AtIndex xs n = AI { getAI :: AtIndex' xs n }

grabIndex :: SListIndex j -> HList xs -> AtIndex' xs j
grabIndex SHere (HCons x _) = x
grabIndex (SThere i) (HCons _ xs) = grabIndex i xs

-- instance Cone (HList xs) (DiscreteDiagram (AtIndex xs)) where
--     indexCone :: forall (j :: ListIndex xs).  SListIndex j -> HList xs -> DiscreteDiagram (AtIndex xs) j
--     indexCone i xs = DD $ AI $ grabIndex i xs

-- data SomeCone1' (d :: k -> j -> Type) (a :: k) = forall c. Cone (c a) (d a) => MkSomeCone1' (c a)

instance Diagram (AtIndex xs) where
    type Arrow (AtIndex xs) = (:~:)
    dmap _ _ Refl = id

instance Cone (HList xs) (AtIndex xs) where
    indexCone :: forall (j :: ListIndex xs). SListIndex j -> HList xs -> AtIndex xs j
    indexCone i xs = AI $ grabIndex i xs

data SomeDepCone1 (d :: forall k -> j k -> Type) a = forall c. Cone (c a) (d a) => MkSomeDepCone1 (c a)
type SomeHlistCone xs = SomeDepCone1 AtIndex

class Cone n d => Limit n d | n -> d where
    getLimit :: Cone n' d => n' -> n

getSomeLimit :: Limit n d => SomeCone d -> n
getSomeLimit (MkSomeCone x) = getLimit x

limitLaw1 :: forall a d n n'. (Eq (d a), Cone n' d, Limit n d) => Sing a -> n' -> Bool
limitLaw1 a x = indexCone a (getLimit @n x) == indexCone a x

-- The limit morphism should be unique
-- Assumes that lim2 also follows limitLaw1
limitLaw2 :: forall d n n'. (Eq n, Cone n' d, Limit n d) => (n' -> n) -> n' -> Bool
limitLaw2 lim2 x = getLimit @n x == lim2 x

instance Limit (SpanCone (,) a b) (DSpan a b) where
  getLimit :: Cone n' (DSpan a b) => n' -> SpanCone (,) a b
  getLimit p = SpanCone (unDS $ indexCone SFalse p, unDS $ indexCone STrue p)


-- instance KnownTypeList xs => Limit (HList xs) (AtIndex xs) where

data ListLength (xs :: [Type]) where
    Empty :: ListLength '[]
    HasElem :: KnownLength xs => ListLength (x : xs)

class KnownLength (xs :: [Type]) where
    lengthSing :: ListLength xs
    -- lengthSing :: SNat (Length xs)

instance KnownLength '[] where
    lengthSing = Empty

instance KnownLength xs => KnownLength (x : xs) where
    lengthSing = HasElem

-- coneToHList :: forall xs n. KnownLength xs => Cone n (AtIndex xs) => n -> HList xs
-- coneToHList c = case lengthSing @xs of
--         Empty   -> HNil
--         HasElem -> HCons (getAI (indexCone SHere c)) (_ $ \x -> indexCone x c)

coneToHList1 :: forall xs. KnownLength xs => (forall j. SListIndex j -> AtIndex xs j) -> HList xs
coneToHList1 f =  case lengthSing @xs of
        Empty   -> HNil
        HasElem -> HCons (getAI (f SHere)) (coneToHList1 $ AI . getAI . f . SThere)

instance KnownLength xs => Limit (HList xs) (AtIndex xs) where
    getLimit :: Cone n' (AtIndex xs) => n' -> HList xs
    getLimit c = coneToHList1 (\j -> indexCone j c)
-- instance KnownLength xs => Limit (HList xs) (DiscreteDiagram (AtIndex xs)) where
--     getLimit :: Cone n' (DiscreteDiagram (AtIndex xs)) => n' -> HList xs
--     getLimit c = coneToHList1 (\j -> getDD $ indexCone (SDC j) c)

class Iso a b where
    cast :: a -> b
    cocast :: b -> a

isoLaw1 :: (Iso a b, Eq a, Eq b) => a -> b -> Bool
isoLaw1 x y = cast x == y && cocast y == x

newtype LimitIso n d = LimitIso { getLimitIso :: n }

instance (Limit n d, Limit n' d) => Iso (LimitIso n d) (LimitIso n' d) where
    cast = LimitIso . getLimit . getLimitIso
    cocast = LimitIso . getLimit . getLimitIso

newtype WrapLimit n = WrapLimit n

-- instance (Limit (n a b) (DSpan a b), Limit (n a b) (DSpan a b), Product s) => Iso (WrapLimit (n a b)) (s a b) where
--     cast :: (Limit (n a b) (DSpan a b), Product s) => WrapLimit (n a b) -> s a b
--     -- cast (WrapLimit x) = pFactor (unDS $ indexCone SFalse x , unDS $ indexCone STrue x)
--     -- cast (WrapLimit x) = _ $ getLimit $ Cs' $ (_, _)
--     cast (WrapLimit x) = unCs' $ pFactor' $ Cs' x
--     cocast :: (Limit (n a b) (DSpan a b), Product s) => s a b -> WrapLimit (n a b)
--     -- cocast x = WrapLimit . getLimit . SpanCone $ (fst x, snd x)
--     cocast = WrapLimit . getLimit . SpanCone

-- newtype TwostarDiag a b = TwostarDiag a
newtype TwostarDiag a b (j :: Bool) = TwostarDiag { getTwostar :: DSpan' a b j }

instance Category (Const2 ()) where
    id = Const2 ()
    _ . _ = Const2 ()
    -- type Arrow Bool (TwostarDiag a b) = Const2 ()

instance Iso a b => Diagram (TwostarDiag a b) where
    type Arrow (TwostarDiag a b) = Const2 ()

    dmap :: SBool j -> SBool k -> Const2 () j k -> TwostarDiag a b (j) -> TwostarDiag a b (k)
    dmap STrue  STrue  (Const2 ()) (TwostarDiag ts) = TwostarDiag $ ts
    dmap STrue  SFalse (Const2 ()) (TwostarDiag ts) = TwostarDiag $ cocast ts
    dmap SFalse STrue  (Const2 ()) (TwostarDiag ts) = TwostarDiag $ cast ts
    dmap SFalse SFalse (Const2 ()) (TwostarDiag ts) = TwostarDiag $ ts

data Limit2Star a b = L2S a

instance Iso a b => Cone (Limit2Star a b) (TwostarDiag a b) where
    indexCone :: Iso a b => Sing j -> (Limit2Star a b) -> TwostarDiag a b j
    indexCone idx (L2S a) = dmap SFalse idx (Const2 ()) (TwostarDiag a)
    -- indexCone SFalse (L2S a) = TwostarDiag a
    -- indexCone STrue  (L2S a) = TwostarDiag $ cast a

instance Iso a b => Limit (Limit2Star a b) (TwostarDiag a b) where
    getLimit :: (Cone n' (TwostarDiag a b)) => n' -> Limit2Star a b
    getLimit other = L2S $ getTwostar $ indexCone SFalse other

data Three = A | B | C

data SThree (n :: Three) where
    SA :: SThree A
    SB :: SThree B
    SC :: SThree C

type instance Sing = SThree

type family Select3 (j :: Three) a b c where
    Select3 A a b c = a
    Select3 B a b c = b
    Select3 C a b c = c

data PullbackDiag a b c (j :: Three) = PbDg { getPbDg :: Select3 j a b c}

-- type family PullbackArrow' (j :: Three) (k :: Three) where
data PullbackArrow (j :: Three) (k :: Three) where
    Id  :: PullbackArrow j j
    A2C :: PullbackArrow A C
    B2C :: PullbackArrow B C


-- newtype PullbackArrow' j k = Pb (PullbackArrow j k)

instance Category PullbackArrow where
    id = Id
    Id . x = x
    x . Id = x

instance Cospan' c a b => Diagram (PullbackDiag a b c) where
    type Arrow (PullbackDiag a b c) = PullbackArrow
    dmap _ _ Id  = id
    dmap _ _ A2C = PbDg . left' . getPbDg
    dmap _ _ B2C = PbDg . right' . getPbDg

class Cospan' c a b => PullbackCone n a b c | n -> a, n -> b, n -> c where
    na :: n -> a
    nb :: n -> b
    cons :: a -> b -> n

nc, nc' :: forall n a b c. (PullbackCone n a b c) => n -> c
nc = left' . na
nc' = right'  . nb

pullbackLaw :: (PullbackCone n a b c, Eq c) => n -> Bool
pullbackLaw n = nc n == nc' n

data PullbackPair a b c = UnsafeMkPullbackPair a b

instance (Eq c, Cospan' c a b) => PullbackCone (PullbackPair a b c) a b c where
    na (UnsafeMkPullbackPair a _) = a
    nb (UnsafeMkPullbackPair _ b) = b
    cons a b | pullbackLaw n = n
             | otherwise = error "invalid pullback"
        where n = (UnsafeMkPullbackPair a b)

newtype PbCn n a b c = PbCn n

instance PullbackCone n a b c => Cone (PbCn n a b c) (PullbackDiag a b c) where
    indexCone :: PullbackCone n a b c => Sing j -> PbCn n a b c -> PullbackDiag a b c j
    indexCone SA (PbCn n) = PbDg $ na n
    indexCone SB (PbCn n) = PbDg $ nb n
    indexCone SC (PbCn n) = PbDg $ nc n

instance PullbackCone n a b c => Limit (PbCn n a b c) (PullbackDiag a b c) where
    getLimit :: (PullbackCone n a b c, Cone n' (PullbackDiag a b c)) => n' -> PbCn n a b c
    getLimit nn = PbCn $ cons (getPbDg $ indexCone SA nn) (getPbDg $ indexCone SB nn)

newtype UnitPullback a = UPb { unUPb :: a}

-- instance Cone (UnitPullback a) (PullbackDiag a b (Either a b)) where
-- instance PullbackCone (UnitPullback a) a a (Either a a) where
--   na = unUPb
--   nb = unUPb
--   cons x y = _

newtype Lengths a b = Lengths Int
instance Cospan' (Lengths a b) [a] [b] where
    left' = Lengths . length
    right' = Lengths . length

class HasDefault a where
    def :: a

instance (HasDefault a, HasDefault b) => Cone (Lengths a b) (PullbackDiag [a] [b] (Lengths a b)) where
    indexCone SA (Lengths lns) = PbDg $ replicate lns def
    indexCone SB (Lengths lns) = PbDg $ replicate lns def
    indexCone SC (Lengths lns) = PbDg $ Lengths lns

-- Law abiding cone, but not law-abiding limit. The limit fails for indexCone SA x /= indexCone SA (getLimit x) if a /= ()
instance (HasDefault a, HasDefault b) => Limit (Lengths a b) (PullbackDiag [a] [b] (Lengths a b)) where
    getLimit :: (Cone n' (PullbackDiag [a] [b] (Lengths a b))) => n' -> Lengths a b
    getLimit n = getPbDg $ indexCone SC n

newtype IdWrap a b = IdW a
newtype IdB a f = IdB (f a)
instance Applicative f => Cospan' (IdB a f) a (f a) where
    left' = IdB . pure
    right' = IdB . id
instance Applicative f => Cone (IdWrap a f) (PullbackDiag a (f a) (IdB a f)) where
    indexCone SA (IdW x) = PbDg $ id x
    indexCone SB (IdW x) = PbDg $ pure x
    indexCone SC (IdW x) = PbDg $ left' x
instance Applicative f => Limit (IdWrap a f) (PullbackDiag a (f a) (IdB a f)) where
    getLimit = IdW . getPbDg . indexCone SA

-- data EmptyCat :: Void -> Void -> Type where

-- instance Category EmptyCat where
--   id =
--   (.) = _

data EmptyDiagram (x :: Void) = EmptyDiagram

data SVoid (v :: Void) where
type instance Sing = SVoid

sabsurd :: SVoid v -> a
sabsurd x = case x of {}

instance Diagram EmptyDiagram where
    type Arrow EmptyDiagram = (:~:)
    dmap :: Sing ja -> Sing jb -> Arrow EmptyDiagram ja jb -> EmptyDiagram ja -> EmptyDiagram jb
    dmap = sabsurd

data Terminal t = Terminal t
instance Cone (Terminal t) EmptyDiagram where
    indexCone sj = sabsurd sj

instance Limit (Terminal ()) EmptyDiagram where
    getLimit :: Cone n' EmptyDiagram => n' -> Terminal ()
    getLimit _ = Terminal ()




-- * Functors from non-hask categories to non-hask categories

class (Category c, Category c') => CFunctor (c :: a -> a -> Type) (c' :: b -> b -> Type) (f :: a -> b) | f -> c , f -> c' where
    cmap :: Sing x -> Sing y -> c x y -> c' (f x) (f y)

data SType :: Type -> Type
type instance Sing = SType

-- data EmptyCat :: Void -> Void -> Type where
--     NoMap :: forall (v :: Void). EmptyCat v v

-- instance Category EmptyCat where
--   id = _
--   (.) = _

instance Functor f => CFunctor (->) (->) f where
    cmap _ _ = fmap


data family EmptyDiagC :: q -> Void -> a

-- data instance EmptyDiagC q v

instance Category x => CFunctor (:~:) x (EmptyDiagC x) where
    cmap v = sabsurd v

class (Category c) => CSpan a (c :: a -> a -> Type) (s :: a) (x :: a) (y :: a) where
    cfst :: c s x
    csnd :: c s y

class (Category c, CSpan a c s x y) => CProd a (c :: a -> a -> Type) (s :: a) (x :: a) (y :: a) where
    cProd :: (CSpan a c s' x y) => c s' s

-- class CCone j ()

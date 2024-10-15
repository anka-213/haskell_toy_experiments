
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module ReflectionExperiments where
import Type.Reflection
import Data.Dynamic
import Data.Type.Equality (type (~~))
import Data.Kind (Type)
import Data.Data (Proxy(..))
-- import Data.Proxy (Proxy)

-- pattern Dyn :: forall a. () => Typeable a => a -> Dynamic;
pattern Dyn :: forall a. Typeable a => a -> Dynamic;
pattern Dyn a <- (fromDynamic -> Just a)
    where Dyn a = toDyn a

-- pattern Dyn2 :: forall a. Typeable a => a -> Dynamic;
-- pattern Dyn2 x <- Dynamic (TypeIs @a) x
--     where Dyn2 x = Dynamic (TypeIs @a) x
-- pattern Dyn2 x = Dynamic (TypeIs @a) x

-- pattern Typ :: Proxy a -> TypeRep a
-- pattern Typ :: Char -> forall (a :: Type) -> Char -> TypeRep a
-- pattern Typ x <- ((\(u :: forall a -> Char -> TypeRep a) -> _ $ u Int 'b') -> x )
-- pattern Typ x <- ((\u x y z -> TypeRep @()) -> x ) -- (typeRep -> ())
-- pattern Typ :: forall a -> TypeRep a
-- pattern Typ x <- (typeRep x -> x)
--     where Typ a = _

-- pattern TypeIs :: forall a b. Typeable a => (a :~~: b) -> TypeRep b
-- pattern TypeIs x <- (eqTypeRep typeRep -> Just x)
-- pattern TypeIs :: forall a -> forall b. Typeable a => (a ~ b) => TypeRep b

pattern TypeIs :: forall a b. Typeable a => (a ~~ b) => TypeRep b
pattern TypeIs <- (eqTypeRep (typeRep @a) -> Just HRefl)
    where TypeIs = TypeRep

pattern App2 :: () => t ~ f a b => TypeRep f -> TypeRep a -> TypeRep b -> TypeRep t
pattern App2 f x y = App (App f x) y

pattern Pair :: () => (t ~ (a , b)) => TypeRep a -> TypeRep b -> TypeRep t
pattern Pair a b = App2 (TypeIs @(,)) a b

pattern TypeKinded :: forall {k} (a :: k). () => (k ~ Type) => TypeRep a -> TypeRep a
-- pattern TypeKinded <- (eqTypeRep (typeRep @Type) . typeRepKind -> Just HRefl)
pattern TypeKinded res <- (\res -> (typeRep @Type `eqTypeRep` typeRepKind res, res) -> (Just HRefl, res))
    where TypeKinded res = res

pattern FunMatch :: forall a. a -> Int -> a
pattern FunMatch x <- (($ 2) -> x)

pattern IsMember, NotMember :: (Foldable t, Eq a) => (a, t a)
pattern IsMember  <- (uncurry elem -> True)
pattern NotMember  <- (uncurry elem -> False)
{-# COMPLETE IsMember, NotMember #-}

answer :: [Int] -> String
answer ((,) 42 -> IsMember) = "We know the answer"
answer ((,) 42 -> NotMember) = "Never mind."
answer _ = "impossible"


example :: Dynamic -> Dynamic
example (Dyn @Int 1) = Dyn True
example (Dyn False) = Dyn "hello"
-- example (Dyn (a,b)) = Dyn "hello"
example (Dynamic (Fun (TypeIs @()) (TypeIs @Int)) f) = Dyn (f ())
example (Dynamic (Fun (TypeIs @()) (TypeKinded res)) f) = Dynamic res (f ())
example (Dynamic (Fun (TypeIs @()) res) f) -- Equivalent to the previous line
    | Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind res = Dynamic res (f ())
-- example (Dynamic (Fun (TypeIs () HRefl) (TypeIs @Int HRefl)) f) = Dyn (f ())
-- example (Dynamic (Fun (TypeIs @() HRefl) x) f) = useFun x f
-- example (Dynamic (App2 (TypeIs @(,)) a b) (x,y)) = Dynamic _ _
example (Dynamic (Pair a b) (x,y)) = Dynamic (Pair b a) (y, x)
example (Dynamic tr _) = Dynamic (App typeRep tr) tr -- Convert a dynamic value to its typeRep

-- useFun :: TypeRep res -> (() -> res) -> Dynamic
-- useFun = _

showDyn :: Int -> Dynamic -> ShowS
showDyn d (Dynamic rep' val') = showDyn' d rep' val'
  where
    showDyn' :: Int -> TypeRep a -> a -> ShowS
    showDyn' n (TypeIs @Int   ) x = showsPrec n x
    showDyn' n (TypeIs @Bool  ) x = showsPrec n x
    showDyn' n (TypeIs @String) x = showsPrec n x
    showDyn' n (Pair a b) (x, y)  = showParen True $ showDyn' n a x . showString ", " . showDyn' n b y -- TODO: Fix precedence (change n)
    showDyn' n rep val            = showsPrec n (Dynamic rep val)
-- showDyn (Dyn @Int x) = show x
-- showDyn (Dyn @Bool x) = show x
-- showDyn (Dyn @String x) = show x
-- showDyn (Dynamic (Pair (TypeIs @Int) (TypeIs @Int)) x) = show x
-- showDyn x = show x

newtype ShowableDyn = SD Dynamic
instance Show ShowableDyn where
    -- show (SD x) = showDyn x
    showsPrec n (SD x) = showDyn n x

-- >>> SD $ Dyn (1 :: Int)
-- >>> SD $ Dyn 'a'
-- >>> SD $ example $ Dyn (1 :: Int)
-- >>> SD $ example $ Dyn (2 :: Int)
-- >>> SD $ example $ Dyn False
-- >>> SD $ example $ Dyn (True, False)
-- >>> SD $ example $ Dyn $ \() -> (3 :: Int)
-- 1
-- <<Char>>
-- True
-- <<TypeRep * Int>>
-- "hello"
-- (False, True)
-- 3

-- {-

data SomeShowTypeRep where
    HasShow :: forall r. (Show r, Typeable r) => SomeShowTypeRep
    HasShow1 :: forall r. (forall a. Show a => Show (r a), Typeable r) => SomeShowTypeRep
    HasShow2 :: forall r. (forall a b. (Show a, Show b) => Show (r a b)) => SomeShowTypeRep
-- hasShow2 :: forall r. (forall a. Show (r a)) => SomeShowTypeRep
-- hasShow2 = undefined

-- Use some similar trick to the one used in the TypeRep  pattern
-- pattern SomeShowTypeRep :: forall r. () => Show r => TypeRep r -> SomeShowTypeRep
-- pattern SomeShowTypeRep rep <- (HasShow -> rep)

showTypes :: [SomeShowTypeRep]
showTypes =
    [ HasShow @()
    , HasShow @Int
    , HasShow @Bool
    , HasShow @String
    , HasShow1 @[]
    , HasShow1 @Maybe
    , HasShow2 @(,)
    ]

-- Maybe split up by arity to avoid traversing all
-- Or use some hashmap or other lookup

typeEq :: forall a -> Typeable a => TypeRep b -> Maybe (a :~~: b)
-- typeEq :: forall {k1} {k2} {a :: k1} {b :: k2}. Typeable a => Proxy a -> TypeRep b -> Maybe (a :~~: b)
typeEq a tb = TypeRep @a `eqTypeRep` tb

pattern Equal :: () => (a ~~ b) => Maybe (a :~~: b)
pattern Equal = Just HRefl

showUsingSomeDict :: SomeShowTypeRep -> TypeRep a -> Maybe (Int -> a -> ShowS)
showUsingSomeDict (HasShow @t) (typeEq t -> Equal) = Just $ showsPrec
-- showUsingSomeDict (HasShow @t) rep
--     | Just HRefl <- TypeRep @t `eqTypeRep` rep = Just $ showsPrec
showUsingSomeDict _ _ = Nothing
-- showUsingSomeDict (HasShow @t) (Dyn @t val) = _

-- pattern Foo :: forall a -> Maybe a
-- pattern Foo x <- Just x

pattern Foo :: forall a b. Show a => Show b => Maybe b -> (a :~~: b, Maybe a)
pattern Foo x <- (HRefl, x)

-- -}

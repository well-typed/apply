{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.Apply.Base where

import Data.Coerce
import Data.Kind
import Data.Proxy

-- | Infix version of 'Function'.
type a ---> b = Function a b
infixr 0 --->

-- | Open kind of type-level function symbols.
type Function a b = FunctionKind a b -> Type

-- | Auxiliary kind, to be used in 'Function'.
data FunctionKind :: Type -> Type -> Type

-- | Newtype intended for use with @DerivingVia@.
--
-- Parameterised by a type-level description of a
-- (possibly invertible) function.
--
-- If classes allow an instance with weaker
-- constraints by not implementing all methods and
-- relying on defaults in a few places, then instances
-- for 'Apply' should choose this option. If you want
-- a full instance definition, use 'Apply'' instead.
--
newtype Apply (f :: a ---> b) = Apply { unApply :: a }

-- | Newtype intended for use with @DerivingVia@.
--
-- Parameterised by a type-level description of a
-- (possibly invertible) function.
--
-- Instances for 'Apply'' should always define all
-- methods and not rely on defaults.
--
newtype Apply' (f :: a ---> b) = Apply' { unApply' :: a }

-- | Interpret a type-level description of a function
-- as an actual term-level function.
--
class CanApply (f :: a ---> b) where
  apply :: proxy f -> a -> b

applyInv :: forall (f :: a ---> b) proxy . CanApply (Inv f) => proxy f -> b -> a
applyInv _ = apply (Proxy @(Inv f))

-- | Description of the identity function.
data Id   :: a ---> a

-- | Description of composition.
data Comp :: (b ---> c) -> (a ---> b) -> (a ---> c)

-- | Description of inversion.
data Inv  :: (a ---> b) -> (b ---> a)

-- | Infix version of 'Comp'.
type (...) = Comp
infixr 9 ...

instance CanApply Id where
  apply _ = id

instance CanApply (Inv Id) where
  apply _ = id

instance (CanApply f, CanApply g) => CanApply (f ... g) where
  apply _ = apply (Proxy @f) . apply (Proxy @g)

instance (CanApply (Inv f), CanApply (Inv g)) => CanApply (Inv (f ... g)) where
  apply _ = apply (Proxy @(Inv g)) . apply (Proxy @(Inv f))

instance CanApply f => CanApply (Inv (Inv f)) where
  apply _ = apply (Proxy @f)

-- | Description of (safe) coercion.
--
-- The types 'a' and 'b' will have to be 'Coercible'.
--
data Coerce (a :: Type) (b :: Type) :: a ---> b

instance Coercible a b => CanApply (Coerce a b) where
  apply _ = coerce

instance Coercible a b => CanApply (Inv (Coerce a b)) where
  apply _ = coerce

-- | Explicitly pair a function with an inverse.
--
data Pair :: (a ---> b) -> (b ---> a) -> (a ---> b)

instance CanApply f => CanApply (Pair f g) where
  apply _ = apply (Proxy @f)

instance CanApply g => CanApply (Inv (Pair f g)) where
  apply _ = apply (Proxy @g)

-- | Description of mapping over a functor.
--
data FMap :: (a ---> b) -> (f a ---> f b)

instance (CanApply m, Functor f) => CanApply (FMap m :: f a ---> f b) where
  apply _ = fmap (apply (Proxy @m))

instance (CanApply (Inv m), Functor f) => CanApply (Inv (FMap m :: f a ---> f b)) where
  apply _ = fmap (apply (Proxy @(Inv m)))

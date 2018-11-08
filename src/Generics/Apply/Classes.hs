{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.Apply.Classes where

import Data.Proxy
import Data.Semigroup
import GHC.Float
import GHC.Read

import Generics.Apply.Base

op0 :: forall (f :: a ---> b) . (CanApply (Inv f)) => b -> Apply f
op0 o = Apply (applyInv (Proxy @f) o)

op1 :: forall (f :: a ---> b) . (CanApply f, CanApply (Inv f)) => (b -> b) -> (Apply f -> Apply f)
op1 o (Apply x) = Apply (applyInv (Proxy @f) (o (apply (Proxy @f) x)))

op1f :: forall (f :: a ---> b) g . (CanApply f, CanApply (Inv f), Functor g) => (b -> g b) -> (Apply f -> g (Apply f))
op1f o (Apply x) = fmap (Apply . applyInv (Proxy @f)) (o (apply (Proxy @f) x))

op1k :: forall (f :: a ---> b) c . (CanApply f) => (b -> c) -> (Apply f -> c)
op1k o (Apply x) = o (apply (Proxy @f) x)

op2 :: forall (f :: a ---> b) . (CanApply f, CanApply (Inv f)) => (b -> b -> b) -> (Apply f -> Apply f -> Apply f)
op2 o (Apply x) (Apply y) = Apply (applyInv (Proxy @f) (o (apply (Proxy @f) x) (apply (Proxy @f) y)))

op2f :: forall (f :: a ---> b) g . (CanApply f, CanApply (Inv f), Functor g) => (b -> b -> g b) -> (Apply f -> Apply f -> g (Apply f))
op2f o (Apply x) (Apply y) = fmap (Apply . applyInv (Proxy @f)) (o (apply (Proxy @f) x) (apply (Proxy @f) y))

op2k :: forall (f :: a ---> b) c . (CanApply f) => (b -> b -> c) -> (Apply f -> Apply f -> c)
op2k o (Apply x) (Apply y) = o (apply (Proxy @f) x) (apply (Proxy @f) y)

op2' :: forall (f :: a ---> b) . (CanApply f, CanApply (Inv f)) => (b -> b -> b) -> (Apply' f -> Apply' f -> Apply' f)
op2' o (Apply' x) (Apply' y) = Apply' (applyInv (Proxy @f) (o (apply (Proxy @f) x) (apply (Proxy @f) y)))

op2k' :: forall (f :: a ---> b) c . (CanApply f) => (b -> b -> c) -> (Apply' f -> Apply' f -> c)
op2k' o (Apply' x) (Apply' y) = o (apply (Proxy @f) x) (apply (Proxy @f) y)

op3 :: forall (f :: a ---> b) . (CanApply f, CanApply (Inv f)) => (b -> b -> b -> b) -> (Apply f -> Apply f -> Apply f -> Apply f)
op3 o (Apply x) (Apply y) (Apply z) = Apply (applyInv (Proxy @f) (o (apply (Proxy @f) x) (apply (Proxy @f) y) (apply (Proxy @f) z)))

op3f :: forall (f :: a ---> b) g . (CanApply f, CanApply (Inv f), Functor g) => (b -> b -> b -> g b) -> (Apply f -> Apply f -> Apply f -> g (Apply f))
op3f o (Apply x) (Apply y) (Apply z) = fmap (Apply . applyInv (Proxy @f)) (o (apply (Proxy @f) x) (apply (Proxy @f) y) (apply (Proxy @f) z))

instance forall (f :: a ---> b) . (CanApply f, Eq b) => Eq (Apply f) where
  (==) = op2k (==)
  (/=) = op2k (/=)

deriving via Apply (f :: a ---> b) instance (CanApply f, Eq b) => Eq (Apply' (f :: a ---> b))

instance forall (f :: a ---> b) . (CanApply f, Ord b) => Ord (Apply f) where
  compare = op2k compare
  (<) = op2k (<)
  (<=) = op2k (<=)
  (>) = op2k (>)
  (>=) = op2k (>=)

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), Ord b) => Ord (Apply' f) where
  compare = op2k' compare
  (<) = op2k' (<)
  (<=) = op2k' (<=)
  (>) = op2k' (>)
  (>=) = op2k' (>=)
  max = op2' max
  min = op2' min

instance forall (f :: a ---> b) . (CanApply f, Show b) => Show (Apply f) where
  showsPrec p = op1k (showsPrec p)
  show = op1k show
  showList xs = showList (fmap (apply (Proxy @f) . unApply) xs)

deriving via Apply (f :: a ---> b) instance (CanApply f, Show b) => Show (Apply' (f :: a ---> b))

instance forall (f :: a ---> b) . (CanApply (Inv f), Read b) => Read (Apply f) where
  readsPrec x y = fmap (\ (a, b) -> (Apply (applyInv (Proxy @f) a), b)) (readsPrec x y)
  readList x = fmap (\ (a, b) -> (fmap (Apply . applyInv (Proxy @f)) a, b)) (readList x)
  readPrec = fmap (Apply . applyInv (Proxy @f)) readPrec
  readListPrec = fmap (fmap (Apply . applyInv (Proxy @f))) readListPrec

instance forall (f :: a ---> b) . (CanApply (Inv f), Bounded b) => Bounded (Apply f) where
  minBound = op0 minBound
  maxBound = op0 maxBound

deriving via Apply (f :: a ---> b) instance (CanApply (Inv f), Bounded b) => Bounded (Apply' (f :: a ---> b))

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), Enum b) => Enum (Apply f) where
  succ = op1 succ
  pred = op1 pred
  toEnum x = Apply (applyInv (Proxy @f) (toEnum x))
  fromEnum (Apply x) = fromEnum (apply (Proxy @f) x)
  enumFrom = op1f enumFrom
  enumFromThen = op2f enumFromThen
  enumFromTo = op2f enumFromTo
  enumFromThenTo = op3f enumFromThenTo

deriving via Apply (f :: a ---> b) instance (CanApply f, CanApply (Inv f), Enum b) => Enum (Apply' (f :: a ---> b))

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), Semigroup b) => Semigroup (Apply f) where
  (<>) = op2 (<>)
  sconcat xs = op0 (sconcat (fmap (apply (Proxy @f) . unApply) xs))
  stimes x = op1 (stimes x)

deriving via Apply (f :: a ---> b) instance (CanApply f, CanApply (Inv f), Semigroup b) => Semigroup (Apply' (f :: a ---> b))

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), Monoid b) => Monoid (Apply f) where
  mempty = op0 mempty
  mappend = op2 mappend
  mconcat xs = op0 (mconcat (fmap (apply (Proxy @f) . unApply) xs))

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), Num b) => Num (Apply f) where
  (+) = op2 (+)
  (-) = op2 (-)
  (*) = op2 (*)
  negate = op1 negate
  abs = op1 abs
  signum = op1 signum
  fromInteger x = op0 (fromInteger x)

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), Real b) => Real (Apply f) where
  toRational = op1k toRational

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), Integral b) => Integral (Apply f) where
  quot = op2 quot
  rem = op2 rem
  div = op2 div
  mod = op2 mod
  quotRem (Apply x) (Apply y) =
    (\ (a, b) -> (Apply (applyInv (Proxy @f) a), Apply (applyInv (Proxy @f) b)))
    (quotRem (apply (Proxy @f) x) (apply (Proxy @f) y))
  divMod (Apply x) (Apply y) =
    (\ (a, b) -> (Apply (applyInv (Proxy @f) a), Apply (applyInv (Proxy @f) b)))
    (divMod (apply (Proxy @f) x) (apply (Proxy @f) y))
  toInteger = op1k toInteger

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), Fractional b) => Fractional (Apply f) where
  (/) = op2 (/)
  recip = op1 recip
  fromRational x = op0 (fromRational x)

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), Floating b) => Floating (Apply f) where
  pi = op0 pi
  exp = op1 exp
  log = op1 log
  sqrt = op1 sqrt
  (**) = op2 (**)
  logBase = op2 logBase
  sin = op1 sin
  cos = op1 cos
  tan = op1 tan
  asin = op1 asin
  acos = op1 acos
  atan = op1 atan
  sinh = op1 sinh
  cosh = op1 cosh
  tanh = op1 tanh
  asinh = op1 asinh
  acosh = op1 acosh
  atanh = op1 atanh
  log1p = op1 log1p
  expm1 = op1 expm1
  log1pexp = op1 log1pexp
  log1mexp = op1 log1mexp

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), RealFrac b) => RealFrac (Apply f) where
  properFraction = op1f properFraction
  truncate = op1k truncate
  round = op1k round
  ceiling = op1k ceiling
  floor = op1k floor

instance forall (f :: a ---> b) . (CanApply f, CanApply (Inv f), RealFloat b) => RealFloat (Apply f) where
  floatRadix = op1k floatRadix
  floatDigits = op1k floatDigits
  floatRange = op1k floatRange
  decodeFloat = op1k decodeFloat
  encodeFloat x y = op0 (encodeFloat x y)
  exponent = op1k exponent
  significand = op1 significand
  scaleFloat x = op1 (scaleFloat x)
  isNaN = op1k isNaN
  isInfinite = op1k isInfinite
  isDenormalized = op1k isDenormalized
  isNegativeZero = op1k isNegativeZero
  isIEEE = op1k isIEEE
  atan2 = op2 atan2

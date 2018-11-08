{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Generics.Apply.GenericLens where

import Data.Generics.Internal.VL.Prism
import Data.Generics.Product.Fields
import Data.Generics.Product.Positions
import Data.Generics.Product.Subtype
import Data.Generics.Product.Typed
import Data.Generics.Product.Types
import Data.Generics.Sum.Constructors
import Data.Generics.Sum.Subtype
import Data.Generics.Sum.Typed
import Data.Kind
import GHC.TypeLits
import Lens.Micro (toListOf)

import Generics.Apply.Base

-- | Description of the selection of a field.
--
data Field (field :: Symbol) (s :: Type) (a :: Type) :: s ---> a

instance HasField' field s a => CanApply (Field field s a) where
  apply _ = getField @field

-- | Description of the selection by position.
--
data Position (pos :: Nat) (s :: Type) (a :: Type) :: s ---> a

instance HasPosition' pos s a => CanApply (Position pos s a) where
  apply _ = getPosition @pos

-- | Description of a record upcast.
--
data Upcast (sub :: Type) (sup :: Type) :: sub ---> sup

instance Subtype sup sub => CanApply (Upcast sub sup) where
  apply _ = upcast

instance Subtype sub sup => CanApply (Inv (Upcast sub sup)) where
  apply _ = upcast

-- | Description of type-based selection.
--
data Typed (s :: Type) (a :: Type) :: s ---> a

instance HasType a s => CanApply (Typed s a) where
  apply _ = getTyped

-- | Description of type-based multi-selection.
--
data Types (s :: Type) (a :: Type) :: s ---> [a]

instance HasTypes s a => CanApply (Types s a) where
  apply _ = toListOf types

-- | Description of (inverse) constructor application.
--
data Constructor (ctor :: Symbol) (s :: Type) (a :: Type) :: s ---> a

instance AsConstructor' ctor s a => CanApply (Inv (Constructor ctor s a)) where
  apply _ = build (_Ctor' @ctor)

-- | Description of constructor extraction.
--
data UnConstructor (ctor :: Symbol) (s :: Type) (a :: Type) :: s ---> Maybe a

instance AsConstructor' ctor s a => CanApply (UnConstructor ctor s a) where
  apply _ = flip (^?) (_Ctor' @ctor)

-- | Description of projection.
--
data Project (sup :: Type) (sub :: Type) :: sup ---> Maybe sub

instance AsSubtype sub sup => CanApply (Project sup sub) where
  apply _ = projectSub

-- | Description of (inverse) injection.
--
data Inject (sup :: Type) (sub :: Type) :: sup ---> sub

instance AsSubtype sub sup => CanApply (Inv (Inject sup sub)) where
  apply _ = injectSub

-- | Description of typed projection.
--
data ProjectTyped (s :: Type) (a :: Type) :: s ---> Maybe a

instance AsType a s => CanApply (ProjectTyped s a) where
  apply _ = projectTyped

-- | Description of (inverse) typed injection.
--
data InjectTyped (s :: Type) (a :: Type) :: s ---> a

instance AsType a s => CanApply (Inv (InjectTyped s a)) where
  apply _ = injectTyped

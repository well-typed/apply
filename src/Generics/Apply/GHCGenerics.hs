{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Generics.Apply.GHCGenerics where

import Data.Kind
import GHC.Generics

import Generics.Apply.Base

-- | Description of moving to the GHC-generic representation type.
--
-- The type 'b' will have to be 'Rep a ()'.
--
data GHCGeneric (a :: Type) :: a ---> b

instance (Generic a, b ~ Rep a ()) => CanApply (GHCGeneric a :: a ---> b) where
  apply _ = from

instance (Generic a, b ~ Rep a ()) => CanApply (Inv (GHCGeneric a) :: b ---> a) where
  apply _ = to

type CoercibleGHCRep a b =
  ((  Inv (GHCGeneric b)
  ... (Coerce (Rep a ()) (Rep b ()))
  ... GHCGeneric a
  ) :: a ---> b)

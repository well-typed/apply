{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Generics.Apply.SOP where

import Data.Kind
import Generics.SOP

import Generics.Apply.Base

-- | Description of moving to the SOP-generic representation type.
--
-- The type 'b' will have to be 'Rep a'.
--
data SOPGeneric (a :: Type) :: a ---> b

instance (Generic a, b ~ Rep a) => CanApply (SOPGeneric a :: a ---> b) where
  apply _ = from

instance (Generic a, b ~ Rep a) => CanApply (Inv (SOPGeneric a) :: b ---> a) where
  apply _ = to

type SameSOPRep a b =
  ((  Inv (SOPGeneric b)
  ... SOPGeneric a
  ) :: a ---> b)

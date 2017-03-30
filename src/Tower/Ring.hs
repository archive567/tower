{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra

module Tower.Ring (
    -- * Ring
    Semiring
  , Ring
  , CRing
  ) where

import Protolude (Double, Float, Int, Integer,Bool(..))
import Data.Functor.Rep
import Tower.Additive
import Tower.Multiplicative
import Tower.Distribution

-- | a semiring
class ( Additive a
      , MultiplicativeAssociative a
      , MultiplicativeUnital a
      , Distribution a) =>
      Semiring a

instance Semiring Double
instance Semiring Float
instance Semiring Int
instance Semiring Integer
instance Semiring Bool
instance (Representable r, Semiring a) => Semiring (r a)

-- | Ring
class ( AdditiveGroup a
      , MultiplicativeAssociative a
      , MultiplicativeUnital a
      , Distribution a) =>
      Ring a

instance Ring Double
instance Ring Float
instance Ring Int
instance Ring Integer
instance Ring Bool
instance (Representable r, Ring a) => Ring (r a)

-- | CRing is a Commutative Ring.  It arises often due to * being defined as only multiplicative commutative, yet fromInteger being a `Integer -> Ring` (and thus not necessarily commutative).
class ( Multiplicative a, Ring a) => CRing a

instance CRing Double
instance CRing Float
instance CRing Int
instance CRing Integer
instance CRing Bool
instance (Representable r, CRing a) => CRing (r a)


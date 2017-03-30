{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- Distribution

module Tower.Distribution (
    -- * Distribution
    Distribution
  ) where

import Protolude (Double, Float, Int, Integer,Bool(..))
import Data.Functor.Rep
import Tower.Additive
import Tower.Multiplicative

-- | Distribution
--
-- > a * (b + c) == a * b + a * c
--
-- > (a + b) * c == a * c + b * c
--
class (
    Additive a
  , MultiplicativeMagma a
  ) => Distribution a

instance Distribution Double
instance Distribution Float
instance Distribution Int
instance Distribution Integer
instance Distribution Bool
instance (Representable r, Distribution a) => Distribution (r a)


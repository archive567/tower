{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra

module Tower.Field (
    Field
  ) where

import Protolude (Double, Float)
import Data.Functor.Rep
import Tower.Additive
import Tower.Multiplicative
import Tower.Distribution
import Tower.Ring

-- | Field
class ( AdditiveGroup a
      , MultiplicativeGroup a
      , Distribution a
      , Ring a) =>
      Field a

instance Field Double
instance Field Float
instance (Representable r, Field a) => Field (r a)


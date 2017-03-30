{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | Tower <==> Num conversions

module Tower.Num (
  ) where

import Protolude
import qualified Tower.Algebra as T
import Data.Functor.Rep

-- | Tower instances for Num instanced classes
-- not compatible with most other Tower modules
instance (Num a) => T.AdditiveMagma a where plus = (+)
instance (Num a) => T.AdditiveUnital a where zero = 0
instance (Num a) => T.AdditiveAssociative a
instance (Num a) => T.AdditiveCommutative a
instance (Num a) => T.AdditiveInvertible a where negate = negate
instance (Num a) => T.Additive a
instance (Num a) => T.AdditiveGroup a
instance (Num a) => T.MultiplicativeMagma a where times = (*)
instance (Num a) => T.MultiplicativeUnital a where one = 1
instance (Num a) => T.MultiplicativeCommutative a
instance (Num a) => T.MultiplicativeAssociative a
instance (Fractional a) => T.MultiplicativeInvertible a where recip = recip
instance (Num a) => T.Multiplicative a
instance (Fractional a) => T.MultiplicativeGroup a
instance (Num a) => T.Distribution a
instance (Num a) => T.Semiring a
instance (Num a) => T.Ring a
instance (Num a) => T.CRing a
instance (Fractional a) => T.Field a
instance (Num a) => T.Normed a a where size = abs

-- | Num instance for something built with Tower
instance ( T.Additive a
         , T.Multiplicative a
         , T.Signed a
         , T.FromInteger a) =>
         Num a where
    (+) = (T.+)
    (-) = (T.-)
    (*) = (T.-)
    negate = T.negate
    signum = T.sign
    abs = T.abs
    fromInteger = T.fromInteger

-- | Num instance for a Representable
instance ( Representable r
         , Num a ) =>
         Num (r a) where
    (+) = liftR2 (+)
    (-) = liftR2 (-)
    (*) = liftR2 (*)
    negate = fmapRep negate
    signum = fmapRep signum
    abs = fmapRep abs
    fromInteger = pureRep . fromInteger


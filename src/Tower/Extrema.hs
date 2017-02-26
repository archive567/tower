{- * Extrema is the collective term for a minimum and maximum.  There are a few alternative synonyms out there, such as:
- interval
- range
- InfSup

-}

module Tower.Extrema
    ( Extrema(..)
    , low
    , mid
    , range
    , high
    , theta
    )
    where

import Protolude
    ( Eq(..)
    , Ord(..)
    , Bool(..)
    , Show(..)
    , Functor(..)
    , Applicative(..)
    , (||)
    , ($))
import Tower.Algebra
import Control.Lens
import Test.QuickCheck

-- | extrema
newtype Extrema a = Extrema { unextrema_ :: (a, a) }
  deriving (Eq, Ord, Show, Functor)

instance (Ord a, Arbitrary a) => Arbitrary (Extrema a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        if a < b
           then pure (Extrema (a, b))
           else pure (Extrema (b, a))

low :: Lens' (Extrema a) a
low = lens (\(Extrema (l,_)) -> l) (\(Extrema (_,u)) l -> Extrema (l,u))

high :: Lens' (Extrema a) a
high = lens (\(Extrema (_,u)) -> u) (\(Extrema (l,_)) u -> Extrema (l,u))

mid ::
    (BoundedField a) =>
    Lens' (Extrema a) a
mid =
    lens
    (\(Extrema (l,u)) -> (l+u)/two)
    (\(Extrema (l,u)) m -> Extrema (m - (u-l)/two, m + (u-l)/two))

range ::
    (BoundedField a) =>
    Lens' (Extrema a) a
range =
    lens
    (\(Extrema (l,u)) -> (u-l))
    (\(Extrema (l,u)) r ->
       Extrema ((l+u)/two - r/two,
                (l+u)/two + r/two))

instance (Ord a) => AdditiveMagma (Extrema a) where
    plus (Extrema (l0,u0)) (Extrema (l1,u1)) = Extrema (min l0 l1, max u0 u1)

instance (Ord a, BoundedField a) => AdditiveUnital (Extrema a) where
    zero = Extrema (infinity,neginfinity)

instance (Ord a) => AdditiveAssociative (Extrema a)
instance (Ord a) => AdditiveCommutative (Extrema a)
instance (Ord a, BoundedField a) => Additive (Extrema a)

-- | times may well be some sort of affine transformation lurking under the hood
instance (Ord a, BoundedField a) => MultiplicativeMagma (Extrema a) where
    times a b = Extrema (m - r/two, m + r/two)
        where
          m = ((view mid a) + (view mid b * view range a))
          r = view range a * view range b

-- -0.25, 0.5 * 1, 2 >>> 0, 1

-- | The unital object derives from:
--
-- view range one = one
-- view mid zero = zero
-- ie (-0.5,0.5)
instance (Ord a, BoundedField a) => MultiplicativeUnital (Extrema a) where
    one = Extrema (negate half, half)

-- | natural interpretation of an Extrema as a number is the mid-point
instance (BoundedField a) =>
    AdditiveHomomorphic (Extrema a) a where
    plushom (Extrema (l,u)) = (l+u) / two

instance (Ord a, BoundedField a) => MultiplicativeAssociative (Extrema a)
instance (Ord a, BoundedField a) => MultiplicativeCommutative (Extrema a)

instance (Ord a, AdditiveGroup a) => Normed (Extrema a) a where
    size (Extrema (l, u)) = u-l

instance (Ord a, BoundedField a) => MultiplicativeInvertible (Extrema a) where
    recip a = case view range a == zero of
      True  -> zero
      False -> Extrema (m - r/two, m + r/two)
        where
          m = negate (view mid a) * recip (view range a)
          r = recip (view range a)

instance (BoundedField a, Ord a) => Multiplicative (Extrema a)
instance (BoundedField a, Ord a) => MultiplicativeGroup (Extrema a)

-- | theta is a bit like infinity
theta :: (AdditiveUnital a) => Extrema a
theta = Extrema (zero, zero)

two :: (BoundedField a) => a
two = one + one

half :: (BoundedField a) => a
half = one / (one + one)

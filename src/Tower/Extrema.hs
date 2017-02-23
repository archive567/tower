{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- * Semigroup has a `Min` instance, hence the use of Minima

-}

module Tower.Extrema
    ( Minima(..)
    , Maxima(..)
    , Extremum(..)
    , zero'
    , infinity'
    )
    where

import Protolude as X hiding
    ( (+)
    , (-)
    , (*)
    , (/)
    , zero
    , negate
    , recip
    , Integral(..)
    , Semiring(..)
    , log
    , logBase
    , exp
    , sqrt
    , (**)
    , abs
    , (^)
    , infinity
    )

import Tower.Algebra
import Control.Lens
import Test.QuickCheck

newtype Minima a = Minima { getMinima :: a }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance (Arbitrary a) => Arbitrary (Minima a) where
    arbitrary = Minima <$> arbitrary

instance Functor Minima where
    fmap f (Minima a) = Minima (f a)

instance Foldable Minima where
  foldMap f (Minima a) = f a

instance Traversable Minima where
  traverse f (Minima a) = Minima <$> f a

instance Applicative Minima where
    pure a = Minima a
    (Minima f) <*> (Minima a) = Minima (f a)

instance Monad Minima where
  (>>) = (*>)
  Minima a >>= f = f a

instance (Bounded a) => Bounded (Minima a) where
    minBound = Minima minBound
    maxBound = Minima maxBound

instance Ord a => Semigroup (Minima a) where
  (Minima a) <> (Minima b) = Minima (min a b)

instance (Ord a, Bounded a) => Monoid (Minima a) where
  mempty = maxBound
  mappend = (<>)

-- | start of the numeric heirarchy, and point of departure compared with the heirarchy of `[Min](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Semigroup.html#t:Min)`.
-- base wimps out very quickly and defines Min a + Min b = Min (a+b)
-- here, we start with `min` as the magma and see where it flows
instance (Ord a) => AdditiveMagma (Minima a) where
    plus (Minima a) (Minima b) = Minima (min a b)

instance (Ord a) => AdditiveAssociative (Minima a)
instance (Ord a) => AdditiveCommutative (Minima a)

-- | a + a = a
instance (Ord a) => AdditiveIdempotent (Minima a)

instance (Ord a, BoundedField a) => AdditiveUnital (Minima a) where
    zero = Minima infinity

-- | we have Minima 1 + Minima 2 = Minima 1
instance (Ord a, BoundedField a) => Additive (Minima a)

-- | Max
newtype Maxima a = Maxima { getMaxima :: a }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance (Arbitrary a) => Arbitrary (Maxima a) where
    arbitrary = Maxima <$> arbitrary

instance Functor Maxima where
    fmap f (Maxima a) = Maxima (f a)

instance Foldable Maxima where
  foldMap f (Maxima a) = f a

instance Traversable Maxima where
  traverse f (Maxima a) = Maxima <$> f a

instance Applicative Maxima where
    pure a = Maxima a
    (Maxima f) <*> (Maxima a) = Maxima (f a)

instance Monad Maxima where
  (>>) = (*>)
  Maxima a >>= f = f a

instance (Bounded a) => Bounded (Maxima a) where
    minBound = Maxima minBound
    maxBound = Maxima maxBound

instance Ord a => Semigroup (Maxima a) where
  (Maxima a) <> (Maxima b) = Maxima (min a b)

instance (Ord a, Bounded a) => Monoid (Maxima a) where
  mempty = maxBound
  mappend = (<>)

instance (Ord a) => AdditiveMagma (Maxima a) where
    plus (Maxima a) (Maxima b) = Maxima (max a b)

instance (Ord a) => AdditiveAssociative (Maxima a)
instance (Ord a) => AdditiveCommutative (Maxima a)

-- | a + a = a
instance (Ord a) => AdditiveIdempotent (Maxima a)

instance (Ord a, BoundedField a) => AdditiveUnital (Maxima a) where
    zero = Maxima neginfinity

-- | we have Maxima 1 + Maxima 2 = Maxima 2
instance (Ord a, BoundedField a) => Additive (Maxima a)

-- | extrema
newtype Extremum a = Extremum { unextremum :: (Minima a, Maxima a) }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, Functor)

instance (Ord a, Arbitrary a) => Arbitrary (Extremum a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        case a < b of
          True -> return $ Extremum (Minima a, Maxima b)
          False -> return $ Extremum (Minima b, Maxima a)

low :: Lens' (Extremum a) a
low = lens (\(Extremum (Minima l,_)) -> l) (\(Extremum (_,u)) l -> Extremum (Minima l,u))

high :: Lens' (Extremum a) a
high = lens (\(Extremum (_,Maxima u)) -> u) (\(Extremum (l,_)) u -> Extremum (l,Maxima u))

data Extremum' a = Extremum' { _mid :: a, _siz :: a}

makeLenses ''Extremum'

extent :: ( AdditiveGroup a
      , AdditiveHomomorphic (Extremum a) a
      , Ord a
      , MultiplicativeGroup a) =>
      Iso' (Extremum a) (Extremum' a)
extent = iso toSizeAv toRange
  where
    toRange (Extremum' mid s) = Extremum (Minima l, Maxima u)
      where
        l = mid - s/(one+one)
        u = mid + s/(one+one)
    toSizeAv e = Extremum' (plushom e) (size e)

instance (Ord a) => AdditiveMagma (Extremum a) where
    plus (Extremum (l,u)) (Extremum (l',u')) = Extremum (l `plus` l', u `plus` u')

instance (Ord a, BoundedField a) => AdditiveUnital (Extremum a) where
    zero = Extremum (zero,zero)

instance (Ord a) => AdditiveAssociative (Extremum a)
instance (Ord a) => AdditiveCommutative (Extremum a)
instance (Ord a, BoundedField a) => Additive (Extremum a)


-- | times is adding the mid-points and timesing the range size
instance (Ord a, AdditiveGroup a, MultiplicativeGroup a) => MultiplicativeMagma (Extremum a) where
    times a b = view (Control.Lens.from extent) $ Extremum'
        (view mid a' + view mid b')
        (view siz a' * view siz b')
      where
        a' = view extent a
        b' = view extent b

-- | given the AdditiveHomomorphic (think average) and Normed formulations a natural multiplicative unital derives from:
--
-- size one = one
-- hom one = zero
-- ie (-0.5,0.5)
instance (Ord a, BoundedField a) => MultiplicativeUnital (Extremum a) where
    one = Extremum (Minima (negate one / (one + one)), Maxima (one / (one + one)))

instance (Additive a, MultiplicativeGroup a) =>
    AdditiveHomomorphic (Extremum a) a where
    plushom (Extremum (Minima l,Maxima u)) = (l+u) / (one + one)

instance (Ord a, AdditiveGroup a, MultiplicativeGroup a) => MultiplicativeAssociative (Extremum a)
instance (Ord a, AdditiveGroup a, MultiplicativeGroup a) => MultiplicativeCommutative (Extremum a)


instance (Ord a, AdditiveGroup a) => Normed (Extremum a) a where
    size (Extremum (Minima l, Maxima u)) = u-l

instance (Ord a, AdditiveGroup a, MultiplicativeGroup a) => MultiplicativeInvertible (Extremum a) where
    recip a = view (Control.Lens.from extent) $ Extremum'
        (negate $ view mid a')
        (recip  $ view siz a')
      where
        a' = view extent a

instance (BoundedField a, Ord a) => Multiplicative (Extremum a)
instance (BoundedField a, Ord a) => MultiplicativeGroup (Extremum a)

-- | a weird version of zero, with:
-- - `recip zero'` providing a stable idea of infinity (but with no valid neginfinity)
-- - MathematicalGroup laws work except for `zero'`
-- - annihilation applies: zero' * a = zero'
--
-- all of which implies we have the wrong additive unital element
-- 
zero' = Extremum {unextremum = (Minima {getMinima = 0},Maxima {getMaxima = 0})}

infinity' :: Extremum Double
infinity' = recip zero'

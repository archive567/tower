{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A vector based on the linear package

module Tower.VectorL where

import qualified Protolude as P
import Protolude
    (Applicative(..), ($), (<$>), (<*>), Functor(..), Show(..), show, Eq(..))
import Tower.Algebra
import GHC.TypeLits
import Data.Vector as V
import Data.Proxy (Proxy(..))
import Test.QuickCheck
import Linear.V
import Linear.Matrix
import qualified Linear.Vector as Linear

toVectorL :: forall a n . (AdditiveUnital a, KnownNat n) => [a] -> V n a
toVectorL l = V $ fromList $ P.take n $ l P.++ P.repeat zero
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

instance (KnownNat n, Arbitrary a, AdditiveUnital a) => Arbitrary (V n a) where
    arbitrary = frequency
        [ (1, pure zero)
        , (9, toVectorL <$> arbitrary)
        ]

binOp :: (KnownNat n) => (a -> a -> a) -> V n a -> V n a -> V n a
binOp mag (V a) (V b) = V $ zipWith mag a b

instance (KnownNat n, AdditiveMagma a) => AdditiveMagma (V n a) where
    plus = binOp plus
instance (KnownNat n, AdditiveAssociative a) => AdditiveAssociative (V n a)
instance (KnownNat n, AdditiveCommutative a) => AdditiveCommutative (V n a)
instance (KnownNat n, AdditiveUnital a) => AdditiveUnital (V n a) where
    zero = pure zero
instance (KnownNat n, AdditiveInvertible a) => AdditiveInvertible (V n a) where
    negate a = fmap negate a
instance (KnownNat n, Additive a) => Additive (V n a)
instance (KnownNat n, AdditiveGroup a) => AdditiveGroup (V n a)
instance (KnownNat n, AdditiveUnital a, AdditiveMagma a) => AdditiveHomomorphic a (V n a) where
    plushom a = pure a

instance (KnownNat n, MultiplicativeMagma a) => MultiplicativeMagma (V n a) where
    times = binOp times
instance (KnownNat n, MultiplicativeAssociative a) => MultiplicativeAssociative (V n a)
instance (KnownNat n, MultiplicativeCommutative a) => MultiplicativeCommutative (V n a)
instance (KnownNat n, AdditiveUnital a, MultiplicativeUnital a) => MultiplicativeUnital (V n a) where
    one = pure one
instance (KnownNat n, MultiplicativeInvertible a) => MultiplicativeInvertible (V n a) where
    recip = fmap recip
instance (KnownNat n, AdditiveUnital a, Multiplicative a) => Multiplicative (V n a)
instance (KnownNat n, AdditiveUnital a, MultiplicativeGroup a) => MultiplicativeGroup (V n a)
instance (KnownNat n, AdditiveUnital a, MultiplicativeUnital a, MultiplicativeMagma a) => MultiplicativeHomomorphic a (V n a) where
    timeshom a = pure a
instance (KnownNat n, Distributive a) => Distributive (V n a)
instance (KnownNat n, Ring a) => Ring (V n a)
instance (KnownNat n, CRing a) => CRing (V n a)
instance (KnownNat n, Field a) => Field (V n a)

instance (KnownNat n, Additive a) => AdditiveBasis (V n) a
instance (KnownNat n, AdditiveGroup a) => AdditiveGroupBasis (V n) a
instance (KnownNat n, Multiplicative a) => MultiplicativeBasis (V n) a
instance (KnownNat n, MultiplicativeGroup a) => MultiplicativeGroupBasis (V n) a

instance (KnownNat n, Additive a) => AdditiveModule (V n) a
instance (KnownNat n, AdditiveGroup a) => AdditiveGroupModule (V n) a
instance (KnownNat n, Multiplicative a) => MultiplicativeModule (V n) a
instance (KnownNat n, MultiplicativeGroup a) => MultiplicativeGroupModule (V n) a

instance (KnownNat n, Integral a) => Integral (V n a) where
    divMod (V a) (V b) = (d,m)
        where
          x = V.zipWith divMod a b
          d = V $ fmap P.fst x
          m = V $ fmap P.snd x

instance (KnownNat n, ExpField a, ExpRing a) => Normed (V n a) a where
    size (V vec) = sqrt $ foldl' (+) zero $ (**(one+one)) <$> vec

instance (KnownNat n, ExpField a) => Metric (V n a) a where
    distance a b = size (a - b)

instance (KnownNat n, ExpField a) => Banach (V n) a

instance (KnownNat n, Field a, P.Bounded a) => BoundedField (V n a)

instance (KnownNat n, ExpRing a) => ExpRing (V n a) where
    logBase = binOp logBase
    (**)  = binOp (**)

instance (KnownNat n, ExpField a) => ExpField (V n a) where
    exp = fmap exp
    log = fmap log

type family Tensor_V k1 k2 where
    Tensor_V (V m a) (V n a) = V m (V n a)
    Tensor_V (V n a) a = V n a

type instance V n a >< b = Tensor_V (V n a) b

instance (ExpField a, AdditiveGroup a, KnownNat n) => Hilbert (V n) a where
    (<.>) a b = foldl' (+) zero x
      where
        (V x) = a * b

instance (Multiplicative a, KnownNat n) => TensorProduct (V n a) where
    (><) a b = fmap (\x -> fmap (*x) b ) a

    timesleft v m = P.undefined -- v *! m
    timesright m v = P.undefined -- m !* v

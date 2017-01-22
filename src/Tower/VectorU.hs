{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}

-- | unboxed vector

module Tower.VectorU
    ( VectorU(..)
    , toVectorU
    )
    where

import qualified Protolude as P
import Protolude
    (Applicative(..), ($), (<$>), (<*>), Functor(..), Show(..), show, Eq(..))
import Tower.Algebra
import GHC.TypeLits
import Data.Vector.Unboxed as V
import Data.Proxy (Proxy(..))
import Test.QuickCheck

-- newtype VectorU n a = VectorU { unvec :: (KnownNat n, Unbox a) => Vector a}
-- | wrapped fixed-size unboxed vector
data VectorU (n :: Nat) a = VectorU { v :: Vector a} deriving (Eq, Show)

instance (KnownNat n, Arbitrary a, Unbox a, AdditiveUnital a) => Arbitrary (VectorU n a) where
    arbitrary = frequency
        [ (1, pure zero)
        , (9, toVectorU <$> arbitrary)
        ]

-- | toVectorU right pads with zeros, if necessary
-- which introduces an extra AdditiveUnital constraint
toVectorU :: forall a n . (AdditiveUnital a, Unbox a, KnownNat n) => [a] -> VectorU (n :: Nat) a
toVectorU l = VectorU $ fromList $ P.take n $ l P.++ P.repeat zero
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

binOp :: (Unbox a) => (a -> a -> a) -> VectorU n a -> VectorU n a -> VectorU n a
binOp mag (VectorU a) (VectorU b) = VectorU $ zipWith mag a b

instance (Unbox a, AdditiveMagma a) => AdditiveMagma (VectorU n a) where
    plus = binOp plus
instance (Unbox a, AdditiveAssociative a) => AdditiveAssociative (VectorU n a)
instance (Unbox a, AdditiveCommutative a) => AdditiveCommutative (VectorU n a)
instance (KnownNat n, Unbox a, AdditiveUnital a) => AdditiveUnital (VectorU n a) where
    zero = toVectorU []
instance (Unbox a, AdditiveInvertible a) => AdditiveInvertible (VectorU n a) where
    negate (VectorU a) = VectorU $ map negate a
instance (KnownNat n, Unbox a, Additive a) => Additive (VectorU n a)
instance (KnownNat n, Unbox a, AdditiveGroup a) => AdditiveGroup (VectorU n a)
instance (KnownNat n, Unbox a, AdditiveUnital a, AdditiveMagma a) => AdditiveHomomorphic a (VectorU n a) where
    plushom a = toVectorU $ P.repeat a
instance (KnownNat n, Unbox a, Additive a) => AdditiveModule a (VectorU n a)

instance (Unbox a, MultiplicativeMagma a) => MultiplicativeMagma (VectorU n a) where
    times = binOp times
instance (Unbox a, MultiplicativeAssociative a) => MultiplicativeAssociative (VectorU n a)
instance (Unbox a, MultiplicativeCommutative a) => MultiplicativeCommutative (VectorU n a)
instance (KnownNat n, Unbox a, AdditiveUnital a, MultiplicativeUnital a) => MultiplicativeUnital (VectorU n a) where
    one = toVectorU $ P.repeat one
instance (Unbox a, MultiplicativeInvertible a) => MultiplicativeInvertible (VectorU n a) where
    recip (VectorU a) = VectorU $ map recip a
instance (KnownNat n, Unbox a, AdditiveUnital a, Multiplicative a) => Multiplicative (VectorU n a)
instance (KnownNat n, Unbox a, AdditiveUnital a, MultiplicativeGroup a) => MultiplicativeGroup (VectorU n a)
instance (KnownNat n, Unbox a, AdditiveUnital a, MultiplicativeUnital a, MultiplicativeMagma a) => MultiplicativeHomomorphic a (VectorU n a) where
    timeshom a = toVectorU $ P.repeat one
instance (KnownNat n, Unbox a, AdditiveUnital a, Multiplicative a) => MultiplicativeModule a (VectorU n a)

instance (KnownNat n, Unbox a, Distributive a) => Distributive (VectorU n a)
instance (KnownNat n, Unbox a, Ring a) => Ring (VectorU n a)

instance (KnownNat n, Unbox a, Integral a) => Integral (VectorU n a) where
    -- toInteger (VectorU v) = VectorU $ map P.toInteger v
    quotRem (VectorU v) = P.undefined -- (VectorU $ map P.fst v', VectorU $ map P.snd v')
        where v' = P.undefined -- map quotRem v
    divMod = P.undefined -- divMod

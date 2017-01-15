{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}

module Tower.UVector
    ( UVector(..)
    , toUVector
    )
    where

import qualified Protolude as P
import Protolude
    (Applicative(..), ($), (<$>), (<*>), Functor(..), Show(..), show, Eq(..))
import Tower
import GHC.TypeLits
import Data.Vector.Unboxed as V
import Data.Proxy (Proxy(..))
import Test.QuickCheck

-- newtype UVector n a = UVector { unvec :: (KnownNat n, Unbox a) => Vector a}

data UVector (n :: Nat) a = UVector { v :: Vector a} deriving (Eq, Show)

instance (KnownNat n, Arbitrary a, Unbox a, AdditiveUnital a) => Arbitrary (UVector n a) where
    arbitrary = frequency
        [ (1, pure zero)
        , (9, toUVector <$> arbitrary)
        ]

-- right pads with zeros if necessary
toUVector :: forall a n . (AdditiveUnital a, Unbox a, KnownNat n) => [a] -> UVector (n :: Nat) a
toUVector l = UVector $ fromList $ P.take n $ l P.++ P.repeat zero
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

-- class (KnownNat n, Unbox a) => UVector' n a
-- instance (KnownNat n, Show a) => Show (UVector n a)
-- instance (KnownNat n, P.Eq a) => P.Eq (UVector n a)

binOp :: (Unbox a) => (a -> a -> a) -> UVector n a -> UVector n a -> UVector n a
binOp mag (UVector a) (UVector b) = UVector $ zipWith mag a b

instance (Unbox a, AdditiveMagma a) => AdditiveMagma (UVector n a) where
    plus = binOp plus
instance (Unbox a, AdditiveAssociative a) => AdditiveAssociative (UVector n a)
instance (Unbox a, AdditiveCommutative a) => AdditiveCommutative (UVector n a)
instance (KnownNat n, Unbox a, AdditiveUnital a) => AdditiveUnital (UVector n a) where
    zero = toUVector []
instance (Unbox a, AdditiveInvertible a) => AdditiveInvertible (UVector n a) where
    negate (UVector a) = UVector $ map negate a
instance (KnownNat n, Unbox a, Additive a) => Additive (UVector n a)
instance (KnownNat n, Unbox a, AdditiveUnital a, AdditiveMagma a) => AdditiveHomomorphic a (UVector n a) where
    plushom a = toUVector $ P.repeat a
instance (KnownNat n, Unbox a, Additive a) => AdditiveModule a (UVector n a)

instance (Unbox a, MultiplicativeMagma a) => MultiplicativeMagma (UVector n a) where
    times = binOp times
instance (Unbox a, MultiplicativeAssociative a) => MultiplicativeAssociative (UVector n a)
instance (Unbox a, MultiplicativeCommutative a) => MultiplicativeCommutative (UVector n a)
instance (KnownNat n, Unbox a, AdditiveUnital a, MultiplicativeUnital a) => MultiplicativeUnital (UVector n a) where
    one = toUVector $ P.repeat one
instance (Unbox a, MultiplicativeInvertible a) => MultiplicativeInvertible (UVector n a) where
    recip (UVector a) = UVector $ map recip a
instance (KnownNat n, Unbox a, AdditiveUnital a, Multiplicative a) => Multiplicative (UVector n a)
instance (KnownNat n, Unbox a, AdditiveUnital a, MultiplicativeUnital a, MultiplicativeMagma a) => MultiplicativeHomomorphic a (UVector n a) where
    timeshom a = toUVector $ P.repeat one
instance (KnownNat n, Unbox a, AdditiveUnital a, Multiplicative a) => MultiplicativeModule a (UVector n a)

instance (KnownNat n, Unbox a, Distributive a) => Distributive (UVector n a)
instance (KnownNat n, Unbox a, Ring a) => Ring (UVector n a)

instance (KnownNat n, Unbox a, Integral a) => Integral (UVector n a) where
    -- toInteger (UVector v) = UVector $ map P.toInteger v
    quotRem (UVector v) = P.undefined -- (UVector $ map P.fst v', UVector $ map P.snd v')
        where v' = P.undefined -- map quotRem v
    divMod = P.undefined -- divMod

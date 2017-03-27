{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A tower based on Representable

module Tower.Rep where

import qualified Protolude as P
import Protolude
    (($), (<$>), Functor(..), Show, Eq(..))
import Data.Foldable (Foldable(..))
import Tower.Algebra as T
import GHC.TypeLits
import qualified Data.Vector as V
import Data.Proxy (Proxy(..))
import Data.Functor.Rep
import Data.Distributive as D

binOp :: (Representable r) => (a -> a -> a) -> r a -> r a -> r a
binOp mag = liftR2 mag

instance (Representable r, AdditiveMagma a) => AdditiveMagma (r a) where
    plus = binOp plus
instance (Representable r, AdditiveAssociative a) => AdditiveAssociative (r a)
instance (Representable r, AdditiveCommutative a) => AdditiveCommutative (r a)
instance (Representable r, AdditiveUnital a) => AdditiveUnital (r a) where
    zero = pureRep zero
instance (Representable r, AdditiveInvertible a) => AdditiveInvertible (r a) where
    negate a = fmapRep negate a
instance (Representable r, Additive a) => Additive (r a)
instance (Representable r, AdditiveGroup a) => AdditiveGroup (r a)
instance (Representable r, AdditiveMagma a) => AdditiveHomomorphic a (r a) where
    plushom a = pureRep a

instance (Representable r, MultiplicativeMagma a) => MultiplicativeMagma (r a) where
    times = binOp times
instance (Representable r, MultiplicativeAssociative a) => MultiplicativeAssociative (r a)
instance (Representable r, MultiplicativeCommutative a) => MultiplicativeCommutative (r a)
instance (Representable r, MultiplicativeUnital a) => MultiplicativeUnital (r a) where
    one = pureRep one
instance (Representable r, MultiplicativeInvertible a) => MultiplicativeInvertible (r a) where
    recip = fmapRep recip
instance (Representable r, Multiplicative a) => Multiplicative (r a)
instance (Representable r, MultiplicativeGroup a) => MultiplicativeGroup (r a)
instance (Representable r, MultiplicativeMagma a) => MultiplicativeHomomorphic a (r a) where
    timeshom a = pureRep a
instance (Representable r, T.Distributive a) => T.Distributive (r a)
instance (Representable r, Ring a) => Ring (r a)
instance (Representable r, CRing a) => CRing (r a)

instance (Representable r, Field a) => Field (r a)

instance (Representable r, Additive a) => AdditiveBasis r a
instance (Representable r, AdditiveGroup a) => AdditiveGroupBasis r a
instance (Representable r, Multiplicative a) => MultiplicativeBasis r a
instance (Representable r, MultiplicativeGroup a) => MultiplicativeGroupBasis r a

instance (Representable r, Additive a) => AdditiveModule r a
instance (Representable r, AdditiveGroup a) => AdditiveGroupModule r a
instance (Representable r, Multiplicative a) => MultiplicativeModule r a
instance (Representable r, MultiplicativeGroup a) => MultiplicativeGroupModule r a

instance (Representable r, Integral a) => Integral (r a) where
    divMod a b = (d,m)
        where
          x = liftR2 divMod a b
          d = fmap P.fst x
          m = fmap P.snd x

instance (P.Foldable r, Representable r, ExpField a, ExpRing a) => Normed (r a) a where
    size r = sqrt $ P.foldl' (+) zero $ (**(one+one)) <$> r

instance (P.Foldable r, Representable r, ExpField a) => Metric (r a) a where
    distance a b = size (a - b)

instance (P.Foldable r, Representable r, ExpField a) => Banach r a

instance (Field a, Representable r) => Bounded (r a) where
    maxBound = one/zero
    minBound = negate (one/zero)

instance (Bounded (r a), P.Foldable r, Representable r, BoundedField a) => BoundedField (r a) where
    isNaN a = P.any isNaN a

instance (Representable r, ExpRing a) => ExpRing (r a) where
    logBase = binOp logBase
    (**)  = binOp (**)

instance (Representable r, ExpField a) => ExpField (r a) where
    exp = fmap exp
    log = fmap log

type family TensorRep k1 k2 where
    TensorRep (r a) (r a) = r (r a)
    TensorRep (r a) a = r a

type instance r a >< b = TensorRep (r a) b

instance (Foldable r, Representable r, ExpField a, Multiplicative a ) => TensorProduct (r a) where
    (><) m n = tabulate (\i -> index m i *. n)
    timesleft v m = tabulate (\i -> v <.> index m i)
    timesright m v = tabulate (\i -> v <.> index m i)

instance (P.Foldable r, Representable r, ExpField a, AdditiveGroup a) => Hilbert r a where
    (<.>) a b = foldl' (+) zero $ liftR2 (*) a b

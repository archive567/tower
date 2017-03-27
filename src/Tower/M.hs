{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A Matrix - singly Represented by an Int pair

module Tower.M where

import qualified Protolude as P
import Protolude
    (($), Functor(..), Show, Eq(..))
import Tower.Algebra as T
import GHC.TypeLits
import qualified Data.Vector as V
import Data.Proxy (Proxy(..))
import Data.Functor.Rep
import Data.Distributive as D
import qualified Tower.V as V

newtype M m n a = M { toVectorM :: V.Vector a } deriving (Functor, Show, Eq, P.Foldable)

instance (KnownNat m, KnownNat n) => D.Distributive (M m n) where
  distribute f = M $ V.generate (n*m) $ \i -> fmap (\(M v) -> V.unsafeIndex v i) f
    where
      m = P.fromInteger $ natVal (Proxy :: Proxy m)
      n = P.fromInteger $ natVal (Proxy :: Proxy n)

instance (KnownNat m, KnownNat n) => Representable (M m n) where
  type Rep (M m n) = (P.Int, P.Int)
  tabulate f = M $ V.generate (m*n) (\x -> f (divMod x (m*n)))
    where
      m = P.fromInteger $ natVal (Proxy :: Proxy m)
      n = P.fromInteger $ natVal (Proxy :: Proxy n)
  index (M xs) (i0,i1) = xs V.! (i0*m + i1)
    where
      m = P.fromInteger $ natVal (Proxy :: Proxy m)

toM :: forall a m n . (AdditiveUnital a, KnownNat m, KnownNat n) => [a] -> M m n a
toM l = M $ V.fromList $ P.take (m*n) $ l P.++ P.repeat zero
  where
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

row :: forall a m n. (KnownNat m, KnownNat n) => P.Int -> M m n a -> V.V n a
row i (M a) = V.V $ V.unsafeSlice (i*m) n a
  where
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

col :: forall a m n. (KnownNat m, KnownNat n) => P.Int -> M m n a -> V.V m a
col i (M a) = V.V $ V.generate m (\x -> a V.! (i+x*n))
  where
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

mmult :: forall m n k a. (CRing a, KnownNat m, KnownNat n, KnownNat k) =>
    M m k a -> M k n a -> M m n a
mmult x y = tabulate (\(i,j) -> row i x <.> col j y)

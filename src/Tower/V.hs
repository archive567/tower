{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wall #-}

-- | A vector based on Representable

module Tower.V where

import qualified Protolude as P
import Protolude
    (($), (<$>), Functor(..), Show, Eq(..), take)

import Data.Distributive as D
import Data.Functor.Rep
import Data.Proxy (Proxy(..))
import GHC.TypeLits
import qualified Data.List as List
import qualified Data.Vector as V
import Test.QuickCheck
import Tower.Additive

newtype V n a = V { toVector :: V.Vector a }
    deriving (Functor, Show, Eq, P.Foldable, P.Ord)

instance (KnownNat n, Arbitrary a, AdditiveUnital a) => Arbitrary (V n a) where
    arbitrary = frequency
        [ (1, P.pure zero)
        , (9, toV <$> vector n)
        ]
      where
        n = P.fromInteger $ natVal (Proxy :: Proxy n)

instance KnownNat n => D.Distributive (V n) where
    distribute f = V $ V.generate n $ \i -> fmap (\(V v) -> V.unsafeIndex v i) f
      where
        n = P.fromInteger $ natVal (Proxy :: Proxy n)

instance KnownNat n => Representable (V n) where
    type Rep (V n) = P.Int
    tabulate = V P.. V.generate n0
      where
        n0 = P.fromInteger $ natVal (Proxy :: Proxy n)
    index (V xs) i = xs V.! i

-- | create a `V (n::Nat) a` padding with zeros if needed
toV :: forall a n . (AdditiveUnital a, KnownNat n) => [a] -> V n a
toV l = V $ V.fromList $ P.take n $ l P.++ P.repeat zero
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

-- | create a V (m::Nat) (V (n::Nat) a), which is the natural form for an outer product
toVV ::
    forall a m n.
    ( AdditiveUnital a
    , AdditiveUnital (V n a)
    , KnownNat m
    , KnownNat n) =>
    [a] -> V m (V n a)
toVV l = toV $ take m (toV <$> splitList)
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    splitList =
        List.unfoldr
        (\b -> case List.splitAt n b of
                ([],_) -> P.Nothing
                x -> P.Just x) $
        l P.++ P.repeat zero
 



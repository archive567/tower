{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A vector based on the linear package

module Tower.MatrixL where

import qualified Protolude as P
import Protolude
    ((.), Applicative(..), ($), (<$>), (<*>), Functor(..), Show(..), show, Eq(..), undefined)
import Tower.Algebra
import GHC.TypeLits
import Data.Vector as V
import Data.Proxy (Proxy(..))
import Test.QuickCheck
import qualified Linear.V as L
import Linear.V (V(..))
import qualified Linear.Matrix as LM
import qualified Linear.Vector as LV
import qualified Tower.VectorL as VL
import qualified Data.List as List
import Control.Category

newtype M m n a = M { toVV :: V m (V n a) } deriving (Eq, Show, Functor)

instance (KnownNat m, KnownNat n) => Applicative (M m n) where
    pure a = M $ V $ V <$> V.replicate (L.reflectDim (Proxy :: Proxy m)) (V.replicate (L.reflectDim (Proxy :: Proxy n)) a)
    M (V as) <*> M (V bs) =
        M $ V $ V <$> V.zipWith (\(V f) (V a) -> V.zipWith ($) f a) as bs

toV :: forall a n . (AdditiveUnital a, KnownNat n) => [a] -> V n a
toV l = V $ fromList $ P.take n $ l P.++ P.repeat zero
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

toM :: forall a m n . (AdditiveUnital a, KnownNat n, KnownNat m) => [a] -> M m n a
toM l = M $ toV (toV <$> splitList n l)
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    splitList n l =
        List.unfoldr
        (\b -> case List.splitAt n b of
                ([],_) -> P.Nothing
                x -> P.Just x) $
        l P.++ P.repeat zero

instance (KnownNat m, KnownNat n, Arbitrary a, AdditiveUnital a) => Arbitrary (M m n a) where
    arbitrary = frequency
        [ (1, pure zero)
        , (9, toM <$> arbitrary)
        ]


binOp :: (KnownNat n) => (a -> a -> a) -> M m n a -> M m n a -> M m n a
binOp mag (M (V a)) (M (V b)) = M $ V $ V.zipWith mag' a b
    where
      mag' (V a') (V b')= V $ V.zipWith mag a' b'

instance (KnownNat m, KnownNat n, AdditiveMagma a) => AdditiveMagma (M m n a) where
    plus = binOp plus
instance (KnownNat m, KnownNat n, AdditiveAssociative a) => AdditiveAssociative (M m n a)
instance (KnownNat m, KnownNat n, AdditiveCommutative a) => AdditiveCommutative (M m n a)

instance (KnownNat m, KnownNat n, AdditiveUnital a) => AdditiveUnital (M m n a) where
    zero = pure zero

instance (KnownNat m, KnownNat n, AdditiveInvertible a) => AdditiveInvertible (M m n a) where
    negate a = fmap negate a

instance (KnownNat m, KnownNat n, Additive a) => Additive (M m n a)
instance (KnownNat m, KnownNat n, AdditiveGroup a) => AdditiveGroup (M m n a)
instance (KnownNat m, KnownNat n, AdditiveUnital a, AdditiveMagma a) => AdditiveHomomorphic a (M m n a) where
    plushom a = pure a

instance (KnownNat m, KnownNat n, MultiplicativeMagma a) => MultiplicativeMagma (M m n a) where
    times = binOp times
instance (KnownNat m, KnownNat n, MultiplicativeAssociative a) => MultiplicativeAssociative (M m n a)
instance (KnownNat m, KnownNat n, MultiplicativeCommutative a) => MultiplicativeCommutative (M m n a)
instance (KnownNat m, KnownNat n, AdditiveUnital a, MultiplicativeUnital a) => MultiplicativeUnital (M m n a) where
    one = pure one
instance (KnownNat m, KnownNat n, MultiplicativeInvertible a) => MultiplicativeInvertible (M m n a) where
    recip = fmap recip
instance (KnownNat m, KnownNat n, AdditiveUnital a, Multiplicative a) => Multiplicative (M m n a)
instance (KnownNat m, KnownNat n, AdditiveUnital a, MultiplicativeGroup a) => MultiplicativeGroup (M m n a)
instance (KnownNat m, KnownNat n, AdditiveUnital a, MultiplicativeUnital a, MultiplicativeMagma a) => MultiplicativeHomomorphic a (M m n a) where
    timeshom a = pure a
instance (KnownNat m, KnownNat n, Distributive a) => Distributive (M m n a)
instance (KnownNat m, KnownNat n, Ring a) => Ring (M m n a)
instance (KnownNat m, KnownNat n, CRing a) => CRing (M m n a)
instance (KnownNat m, KnownNat n, Field a) => Field (M m n a)

instance (KnownNat m, KnownNat n, Additive a) => AdditiveBasis (M m n) a
instance (KnownNat m, KnownNat n, AdditiveGroup a) => AdditiveGroupBasis (M m n) a
instance (KnownNat m, KnownNat n, Multiplicative a) => MultiplicativeBasis (M m n) a
instance (KnownNat m, KnownNat n, MultiplicativeGroup a) => MultiplicativeGroupBasis (M m n) a

instance (KnownNat m, KnownNat n, Additive a) => AdditiveModule (M m n) a
instance (KnownNat m, KnownNat n, AdditiveGroup a) => AdditiveGroupModule (M m n) a
instance (KnownNat m, KnownNat n, Multiplicative a) => MultiplicativeModule (M m n) a
instance (KnownNat m, KnownNat n, MultiplicativeGroup a) => MultiplicativeGroupModule (M m n) a

instance (KnownNat m, KnownNat n, ExpField a, ExpRing a) => Normed (M m n a) a where
    size (M vec) = sqrt $ foldl' (+) zero $ toVector $ (**(one+one)) P.. size <$> vec

instance (KnownNat m, KnownNat n, ExpField a) => Metric (M m n a) a where
    distance a b = size (a - b)

instance (KnownNat m, KnownNat n, ExpField a) => Banach (M m n) a

instance (Applicative (M m n), BoundedField a) => P.Bounded (M m n a) where
    minBound = pure (negate infinity)
    maxBound = pure infinity

instance (BoundedField a, KnownNat m, KnownNat n, Field a, P.Bounded a) => BoundedField (M m n a)

instance (KnownNat m, KnownNat n, ExpRing a) => ExpRing (M m n a) where
    logBase = binOp logBase
    (**)  = binOp (**)

instance (KnownNat m, KnownNat n, ExpField a) => ExpField (M m n a) where
    exp = fmap exp
    log = fmap log

transpose :: (KnownNat m, KnownNat n) => M m n a -> M n m a
transpose (M m) = M $ LM.transpose m

row :: P.Int -> M m n a -> V n a
row n (M (V vv)) = vv ! n

col :: (KnownNat m, KnownNat n) => P.Int -> M m n a -> V m a
col x a = row x $ transpose a

dim :: (KnownNat m, KnownNat n) => M m n a -> (P.Int, P.Int)
dim m@(M vv) = (L.dim vv, L.dim $ row 0 m)

mmult :: (ExpField a, KnownNat m, KnownNat n, KnownNat k) => M m k a -> M k n a -> M m n a
mmult a b = M $ toV [toV [row i a <.> col j b | j <- [0..(P.snd (dim b) - 1)]] | i <- [0..(P.fst (dim a) - 1)]]

type family Tensor_M a b where
     Tensor_M (M m k r) (M k n r) = M m n r
     Tensor_M (M m n r) r = M m n r

type instance M m n a >< b = Tensor_M (M m n a) b

instance (ExpField a, Multiplicative a, KnownNat n) => TensorProduct (M n n a) where
    (><) a b = mmult a b
    timesleft v m = undefined -- v *! m
    timesright m v = undefined -- m !* v


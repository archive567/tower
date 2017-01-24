{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}

-- | Applicative-style vector
module Tower.VectorA
    ( VectorA(..)
    )
    where

import qualified Protolude as P
import Protolude (Applicative(..), ($), (<$>), (<*>), Functor(..), Show(..), show, Eq(..), Traversable(..))
import Tower.Algebra
import GHC.TypeLits
import GHC.Show
import Test.QuickCheck

-- | a wrapped fixed-size traversable container
newtype VectorA n f a = VectorA { unvec :: (P.Traversable f, KnownNat n, Applicative f, Functor f) => f a}

instance (KnownNat n, Traversable f, Applicative f, Eq (f a)) => Eq (VectorA n f a) where
    (==) (VectorA v) (VectorA v') = v == v'

instance (KnownNat n, Traversable f, Applicative f, Show (f a)) => Show (VectorA n f a) where
    show (VectorA v) = GHC.Show.show v

instance (P.Num a, AdditiveUnital a, Arbitrary a) => Arbitrary (VectorA 5 [] a) where
    arbitrary = frequency
        [ (1, pure $ VectorA $ P.replicate 5 zero)
        , (9, pure $ VectorA [1,2,3,4,5])
        ]

data Supply s v = Supply { unSupply :: [s] -> ([s],v) }
 
instance Functor (Supply s) where 
  fmap f av = Supply (\l -> let (l',v) = unSupply av l in (l',f v))
 
instance Applicative (Supply s) where
  pure v    = Supply (\l -> (l,v))
  af <*> av = Supply (\l -> let (l',f)  = unSupply af l
                                (l'',v) = unSupply av l'
                            in (l'',f v))
 
runSupply :: Supply s v -> [s] -> v
runSupply av l = P.snd $ unSupply av l
 
supply :: Supply s s
supply = Supply (\(x:xs) -> (xs,x))
 
zipWithTF :: (P.Traversable t, P.Foldable f) => (a -> b -> c) -> t a -> f b -> t c
zipWithTF g t f = runSupply  (P.traverse (\a -> g a <$> supply) t) (P.toList f)

binOp :: (a -> a -> a) -> VectorA n f a -> VectorA n f a -> VectorA n f a
binOp mag (VectorA a) (VectorA b) = VectorA $ zipWithTF mag a b

instance (AdditiveMagma a) => AdditiveMagma (VectorA n f a) where
    plus = binOp plus
instance (AdditiveAssociative a) => AdditiveAssociative (VectorA n f a)
instance (AdditiveCommutative a) => AdditiveCommutative (VectorA n f a)
instance (AdditiveUnital a, KnownNat n) => AdditiveUnital (VectorA n [] a) where
    zero = VectorA $ P.replicate n zero
      where
            n = P.fromInteger $ natVal (P.Proxy :: P.Proxy n)
instance (AdditiveInvertible a) => AdditiveInvertible (VectorA n f a) where
    negate (VectorA a) = VectorA $ negate <$> a
instance (Additive a, KnownNat n) => Additive (VectorA n [] a)
instance (AdditiveGroup a, KnownNat n) => AdditiveGroup (VectorA n [] a)
instance (AdditiveMagma a, KnownNat n) => AdditiveHomomorphic a (VectorA n [] a) where
    plushom a = VectorA $ P.replicate n a
      where
            n = P.fromInteger $ natVal (P.Proxy :: P.Proxy n)
instance (Additive a, KnownNat n) => AdditiveModule a (VectorA n [] a)

instance (MultiplicativeMagma a) => MultiplicativeMagma (VectorA n f a) where
    times = binOp times
instance (MultiplicativeAssociative a) => MultiplicativeAssociative (VectorA n f a)
instance (MultiplicativeCommutative a) => MultiplicativeCommutative (VectorA n f a)
instance (MultiplicativeUnital a, KnownNat n) => MultiplicativeUnital (VectorA n [] a) where
    one = VectorA $ P.replicate n one
      where
            n = P.fromInteger $ natVal (P.Proxy :: P.Proxy n)
instance (MultiplicativeInvertible a) => MultiplicativeInvertible (VectorA n f a) where
    recip (VectorA a) = VectorA $ recip <$> a
instance (Multiplicative a, KnownNat n) => Multiplicative (VectorA n [] a)
instance (MultiplicativeMagma a) => MultiplicativeHomomorphic a (VectorA n f a) where
    timeshom a = VectorA (pure a)
instance (Multiplicative a, KnownNat n) => MultiplicativeModule a (VectorA n [] a)

instance (Distributive a, KnownNat n) => Distributive (VectorA n [] a)

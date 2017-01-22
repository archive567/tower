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
        [ (1, pure zero)
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
instance (AdditiveUnital a) => AdditiveUnital (VectorA n f a) where
    zero = VectorA $ pure zero
instance (AdditiveInvertible a) => AdditiveInvertible (VectorA n f a) where
    negate (VectorA a) = VectorA $ negate <$> a
instance (Additive a) => Additive (VectorA n f a)
instance (AdditiveGroup a) => AdditiveGroup (VectorA n f a)
instance (AdditiveMagma a) => AdditiveHomomorphic a (VectorA n f a) where
    plushom a = VectorA (pure a)
instance (Additive a) => AdditiveModule a (VectorA n f a)

instance (MultiplicativeMagma a) => MultiplicativeMagma (VectorA n f a) where
    times = binOp times
instance (MultiplicativeAssociative a) => MultiplicativeAssociative (VectorA n f a)
instance (MultiplicativeCommutative a) => MultiplicativeCommutative (VectorA n f a)
instance (MultiplicativeUnital a) => MultiplicativeUnital (VectorA n f a) where
    one = VectorA $ pure one
instance (MultiplicativeInvertible a) => MultiplicativeInvertible (VectorA n f a) where
    recip (VectorA a) = VectorA $ recip <$> a
instance (Multiplicative a) => Multiplicative (VectorA n f a)
instance (MultiplicativeMagma a) => MultiplicativeHomomorphic a (VectorA n f a) where
    timeshom a = VectorA (pure a)
instance (Multiplicative a) => MultiplicativeModule a (VectorA n f a)

instance (Distributive a) => Distributive (VectorA n f a)

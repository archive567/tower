{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Tower.Vector
    ( Vector(..)
    , show'
    , print'
    )
    where

import qualified Protolude as P
import Protolude (Applicative(..), ($), (<$>), (<*>), Functor(..), Show(..), show)
import Tower
import GHC.TypeLits

{-
vectors
---

vectors, using a wrapped Applicative

-}

newtype Vector n f a = Vector { unvec :: (P.Traversable f, KnownNat n, Applicative f, Functor f) => f a}

show' :: (KnownNat n, P.Traversable f, Applicative f, Show (f a)) => Vector n f a -> P.Text
show' (Vector a) = show a

print' :: (KnownNat n, P.Traversable f, Applicative f, Show (f a)) => Vector n f a -> P.IO ()
print' a = P.putStrLn (show' a)

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

binOp :: (a -> a -> a) -> Vector n f a -> Vector n f a -> Vector n f a
binOp mag (Vector a) (Vector b) = Vector $ zipWithTF mag a b

instance (AdditiveMagma a) => AdditiveMagma (Vector n f a) where
    plus = binOp plus
instance (AdditiveAssociative a) => AdditiveAssociative (Vector n f a)
instance (AdditiveCommutative a) => AdditiveCommutative (Vector n f a)
instance (AdditiveUnital a) => AdditiveUnital (Vector n f a) where
    zero = Vector $ pure zero
instance (AdditiveInvertible a) => AdditiveInvertible (Vector n f a) where
    negate (Vector a) = Vector $ negate <$> a
instance (Additive a) => Additive (Vector n f a)
instance (AdditiveMagma a) => AdditiveHomomorphic a (Vector n f a) where
    plushom a = Vector (pure a)
instance (Additive a) => AdditiveModule a (Vector n f a)

instance (MultiplicativeMagma a) => MultiplicativeMagma (Vector n f a) where
    times = binOp times
instance (MultiplicativeAssociative a) => MultiplicativeAssociative (Vector n f a)
instance (MultiplicativeCommutative a) => MultiplicativeCommutative (Vector n f a)
instance (MultiplicativeUnital a) => MultiplicativeUnital (Vector n f a) where
    one = Vector $ pure one
instance (MultiplicativeInvertible a) => MultiplicativeInvertible (Vector n f a) where
    recip (Vector a) = Vector $ recip <$> a
instance (Multiplicative a) => Multiplicative (Vector n f a)
instance (MultiplicativeMagma a) => MultiplicativeHomomorphic a (Vector n f a) where
    timeshom a = Vector (pure a)
instance (Multiplicative a) => MultiplicativeModule a (Vector n f a)

instance (Distributive a) => Distributive (Vector n f a)


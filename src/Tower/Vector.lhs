> {-# OPTIONS_GHC -fno-warn-missing-methods #-}
> {-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
> 
> module Tower.Vector
>     ( Vector(..)
>     )
>     where
> 
> import qualified Protolude as P
> import Protolude (Applicative(..), ($), (<$>), (<*>))
> import Tower
> 

vectors
---

vectors, using a wrapped Applicative
 
> newtype Vector f a = Vector { unvec :: P.Applicative f => f a}
> 
> type instance Scalar (Vector f a) = Scalar a
> 
> -- type instance Index (Vector n r) = Int
> -- type instance Elem (Vector n r) = Scalar r
> -- type instance SetElem (Vector n r) b = Vector n b
>
> instance (Semigroup a) => Semigroup (Vector f a) where
>     (Vector a) <> (Vector b) = Vector $ (<>) <$> a <*> b
> instance (Monoid a) => Monoid (Vector f a) where
>     mempty = Vector $ pure mempty
> instance (Cancellative a) => Cancellative (Vector f a)
> instance (Group a) => Group (Vector f a) where
>     (Vector a) - (Vector b) = Vector $ (-) <$> a <*> b
>     negate (Vector a) = Vector $ negate <$> a
> instance (Abelian a) => Abelian (Vector f a)
> instance (Times a) => Times (Vector f a) where
>     (Vector a) * (Vector b) = Vector $ (*) <$> a <*> b
> instance (Semiring a) => Semiring (Vector f a) where
>     one = Vector $ pure one
> instance (Ring a) => Ring (Vector f a) where
>     fromInteger a = Vector $ pure (fromInteger a)
> instance (Field a) => Field (Vector f a) where 
>     (Vector a) / (Vector b) = Vector $ (/) <$> a <*> b
>     fromRational a = Vector $ pure $ fromRational a



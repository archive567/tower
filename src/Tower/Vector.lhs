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
>
> 


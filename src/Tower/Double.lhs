> {-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
> 
> module Tower.Double where
>  
> import Tower
> import Protolude (Double(..), ($), (>), (<), (>=), (<=), (==))
> import qualified Protolude as P
> 
> 
> instance AdditiveMagma Double where plus = (P.+)
> instance AdditiveAssociative Double
> instance AdditiveCommutative Double
> instance AdditiveUnital Double where plusunit = 0
> -- instance AdditiveHomomorphic Double Double where plushom x = x
> instance AdditiveInvertible Double where plusinv = P.negate
> instance Additive Double 
> instance AdditiveModule Double Double 
> instance MultiplicativeMagma Double where times = (P.*)
> instance MultiplicativeAssociative Double
> instance MultiplicativeCommutative Double
> instance MultiplicativeUnital Double where timesunit = 0
> -- instance MultiplicativeHomomorphic Double Double where timeshom x = x
> instance MultiplicativeInvertible Double where timesinv = P.recip
> instance Multiplicative Double
> instance MultiplicativeModule Double Double 
>
> instance Distributive Double
> 

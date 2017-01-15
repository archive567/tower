module Tower.Float where

import Tower
import Protolude (Float(..))
import qualified Protolude as P

instance AdditiveMagma Float where plus = (P.+)
instance AdditiveUnital Float where zero = 0
instance AdditiveAssociative Float
instance AdditiveCommutative Float
instance AdditiveInvertible Float where negate = P.negate
instance AdditiveIdempotent Float
instance AdditiveHomomorphic Float Float where plushom x = x

instance MultiplicativeMagma Float where times = (P.*)
instance MultiplicativeUnital Float where one = 1
instance MultiplicativeAssociative Float
instance MultiplicativeCommutative Float
instance MultiplicativeInvertible Float where recip = P.recip
instance MultiplicativeIdempotent Float
instance MultiplicativeHomomorphic Float Float where timeshom x = x

instance Additive Float
instance Multiplicative Float
instance Distributive Float

instance Ring Float 

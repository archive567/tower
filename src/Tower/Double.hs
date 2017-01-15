module Tower.Double where

import Tower
import Protolude (Double(..))
import qualified Protolude as P

instance AdditiveMagma Double where plus = (P.+)
instance AdditiveUnital Double where zero = 0
instance AdditiveAssociative Double
instance AdditiveCommutative Double
instance AdditiveInvertible Double where negate = P.negate
instance AdditiveIdempotent Double
instance AdditiveHomomorphic Double Double where plushom x = x

instance MultiplicativeMagma Double where times = (P.*)
instance MultiplicativeUnital Double where one = 1
instance MultiplicativeAssociative Double
instance MultiplicativeCommutative Double
instance MultiplicativeInvertible Double where recip = P.recip
instance MultiplicativeIdempotent Double
instance MultiplicativeHomomorphic Double Double where timeshom x = x

instance Additive Double
instance Multiplicative Double
instance Distributive Double

instance Ring Double 

module Tower.Int where

import Tower
import Protolude (Int(..))
import qualified Protolude as P

instance AdditiveMagma Int where plus = (P.+)
instance AdditiveUnital Int where zero = 0
instance AdditiveAssociative Int
instance AdditiveCommutative Int
instance AdditiveInvertible Int where negate = P.negate
instance AdditiveIdempotent Int
instance AdditiveHomomorphic Int Int where plushom x = x

instance MultiplicativeMagma Int where times = (P.*)
instance MultiplicativeUnital Int where one = 1
instance MultiplicativeAssociative Int
instance MultiplicativeCommutative Int
instance MultiplicativeIdempotent Int
instance MultiplicativeHomomorphic Int Int where timeshom x = x

instance Additive Int
instance Multiplicative Int
instance Distributive Int

instance Integral Int where
    toInteger = P.toInteger
    quotRem = P.quotRem
    divMod = P.divMod

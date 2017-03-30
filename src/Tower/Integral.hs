{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra

module Tower.Integral (
    -- * Integral
    Integral(..)
  , ToInteger(..)
  , FromInteger(..)
  , fromIntegral
  ) where

import qualified Protolude as P
import Protolude (Double, Float, Int, Integer, Functor(..), ($), (.), Foldable(..), fst, snd, foldr, const, Ord(..))
import Data.Functor.Rep
import Tower.Additive
import Tower.Multiplicative
import Tower.Ring

-- | Integral
--
-- > b == zero || b * (a `div` b) + (a `mod` b) == a
--
class (Ring a) => Integral a where

    infixl 7 `div`, `mod`

    -- | truncates towards negative infinity
    div :: a -> a -> a
    div a1 a2 = fst (divMod a1 a2)
    mod :: a -> a -> a
    mod a1 a2 = snd (divMod a1 a2)

    divMod :: a -> a -> (a,a)

instance Integral Int where divMod = P.divMod
instance Integral Integer where divMod = P.divMod

instance (Representable r, Integral a) => Integral (r a) where
    divMod a b = (d,m)
        where
          x = liftR2 divMod a b
          d = fmap fst x
          m = fmap snd x

class (Integral a) => ToInteger a where
    toInteger :: a -> Integer

class (Ring a) => FromInteger a where
    fromInteger :: Integer -> a
    fromInteger = slowFromInteger

slowFromInteger :: (Ring r) => Integer -> r
slowFromInteger i = if i > zero
                    then foldr (+) zero $ fmap (const one) [one..i]
                    else negate $ foldr (+) zero $ fmap (const one) [one..negate i]

fromIntegral :: (ToInteger a, FromInteger b) => a -> b
fromIntegral = fromInteger . toInteger

instance FromInteger Double where fromInteger = P.fromInteger
instance FromInteger Float where fromInteger = P.fromInteger
instance FromInteger Int where fromInteger = P.fromInteger
instance FromInteger Integer where fromInteger = P.fromInteger

instance ToInteger Int where toInteger = P.toInteger
instance ToInteger Integer where toInteger = P.toInteger


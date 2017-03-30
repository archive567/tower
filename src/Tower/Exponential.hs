{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra

module Tower.Exponential (
    -- * Exponential
    ExpRing(..)
  , (^)
  , ExpField(..)
  ) where

import qualified Protolude as P
import Protolude (Double, Float, Functor(..))
import Data.Functor.Rep
import Tower.Field
import Tower.Multiplicative
import Tower.Additive
import Tower.Ring

-- | ExpRing
class Ring a => ExpRing a where
    logBase :: a -> a -> a
    (**) :: a -> a -> a

-- | (^)
(^) :: ExpRing a => a -> a -> a
(^) = (**)

instance ExpRing Double where
    logBase = P.logBase
    (**) = (P.**)
instance ExpRing Float where
    logBase = P.logBase
    (**) = (P.**)
instance (Representable r, ExpRing a) => ExpRing (r a) where
    logBase = liftR2 logBase
    (**)  = liftR2 (**)

-- | ExpField
class ( Field a
      , ExpRing a ) =>
      ExpField a where
    sqrt :: a -> a
    sqrt a = a**(one/(one+one))

    exp :: a -> a
    log :: a -> a

instance ExpField Double where
    exp = P.exp
    log = P.log

instance ExpField Float where
    exp = P.exp
    log = P.log

instance (Representable r, ExpField a) => ExpField (r a) where
    exp = fmap exp
    log = fmap log


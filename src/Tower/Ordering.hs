{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra

module Tower.Ordering (
    -- * lattice
    POrd(..)
  , POrdering(..)
  , Topped(..)
  , Bottomed(..)
  , Bounded
  , Negated(..)
  , Semilattice
  , Lattice(..)
  , ord2pord
  ) where

import qualified Protolude as P
import Protolude (Double, Float, Int, Integer, Functor(..), ($), (.), (<$>), Foldable(..), fst, snd, foldr, const, Bool(..), Ord(..), Eq(..), any)
import Data.Functor.Rep
import Data.Coerce
import Tower.Magma

-- | Equal to, Less than, Greater than, Not comparable to
data POrdering = PEQ | PLT | PGT | PNC

class POrd s where pcompare :: s -> s -> POrdering

instance (Ord a) => POrd a where
    pcompare n m = if n > m then PGT else if n == m then PEQ else PLT

ord2pord :: P.Ordering -> POrdering
ord2pord P.EQ = PEQ
ord2pord P.LT = PLT
ord2pord P.GT = PGT

class POrd s => Topped s where top :: s
class POrd s => Bottomed s where bottom :: s

class ( Associative a
      , Commutative a
      , Idempotent a) =>
      Semilattice a

class ( Topped a
      , Bottomed a) =>
      Bounded a

instance Topped Int where top = P.maxBound
instance Bottomed Int where bottom = P.minBound
instance Bounded Int

instance Topped Bool where top = True
instance Bottomed Bool where bottom = False
instance Bounded Bool

class (
    Coercible a (Sup a)
  , Coercible a (Inf a)
  , Semilattice (Sup a)
  , Semilattice (Inf a)
  , POrd a
  ) => Lattice a where
    type family Inf a
    type family Sup a
    (/\) :: a -> a -> a
    (/\) = coerce ((⊕) :: Sup a -> Sup a -> Sup a)
    (\/) :: a -> a -> a
    (\/) = coerce ((⊕) :: Inf a -> Inf a -> Inf a)

class (Lattice a, Isomorphic (Inf a) (Sup a) ) => Negated a where
    negated :: a -> a
    negated a = coerce (fst iso (coerce a :: Inf a) :: Sup a) :: a

-- Int
newtype InfInt = InfInt Int
newtype SupInt = SupInt Int

instance Magma InfInt where
    InfInt a ⊕ InfInt b = InfInt (if a <= b then a else b)

instance Magma SupInt where
    SupInt a ⊕ SupInt b = SupInt (if a >= b then a else b)

instance Associative InfInt
instance Associative SupInt

instance Commutative SupInt
instance Commutative InfInt

instance Idempotent SupInt
instance Idempotent InfInt

instance Homomorphic SupInt InfInt where hom (SupInt a) = InfInt (-a)
instance Homomorphic InfInt SupInt where hom (InfInt a) = SupInt (-a)

instance Isomorphic SupInt InfInt where iso = (hom, hom)
instance Isomorphic InfInt SupInt where iso = (hom, hom)

instance Semilattice SupInt
instance Semilattice InfInt

instance Lattice Int where
    type Inf Int = InfInt
    type Sup Int = SupInt

-- Integer
newtype InfInteger = InfInteger Integer
newtype SupInteger = SupInteger Integer

instance Magma InfInteger where
    InfInteger a ⊕ InfInteger b = InfInteger (if a <= b then a else b)

instance Magma SupInteger where
    SupInteger a ⊕ SupInteger b = SupInteger (if a >= b then a else b)

instance Associative InfInteger
instance Associative SupInteger

instance Commutative SupInteger
instance Commutative InfInteger

instance Idempotent SupInteger
instance Idempotent InfInteger

instance Homomorphic SupInteger InfInteger where hom (SupInteger a) = InfInteger (-a)
instance Homomorphic InfInteger SupInteger where hom (InfInteger a) = SupInteger (-a)

instance Isomorphic SupInteger InfInteger where iso = (hom, hom)
instance Isomorphic InfInteger SupInteger where iso = (hom, hom)

instance Semilattice SupInteger
instance Semilattice InfInteger

instance Lattice Integer where
    type Inf Integer = InfInteger
    type Sup Integer = SupInteger

-- Float
newtype InfFloat = InfFloat Float
newtype SupFloat = SupFloat Float

instance Magma InfFloat where
    InfFloat a ⊕ InfFloat b = InfFloat (if a <= b then a else b)

instance Magma SupFloat where
    SupFloat a ⊕ SupFloat b = SupFloat (if a >= b then a else b)

instance Associative InfFloat
instance Associative SupFloat

instance Commutative SupFloat
instance Commutative InfFloat

instance Idempotent SupFloat
instance Idempotent InfFloat

instance Homomorphic SupFloat InfFloat where hom (SupFloat a) = InfFloat (-a)
instance Homomorphic InfFloat SupFloat where hom (InfFloat a) = SupFloat (-a)

instance Isomorphic SupFloat InfFloat where iso = (hom, hom)
instance Isomorphic InfFloat SupFloat where iso = (hom, hom)

instance Semilattice SupFloat
instance Semilattice InfFloat

instance Lattice Float where
    type Inf Float = InfFloat
    type Sup Float = SupFloat

-- Double
newtype InfDouble = InfDouble Double
newtype SupDouble = SupDouble Double

instance Magma InfDouble where
    InfDouble a ⊕ InfDouble b = InfDouble (if a <= b then a else b)

instance Magma SupDouble where
    SupDouble a ⊕ SupDouble b = SupDouble (if a >= b then a else b)

instance Associative InfDouble
instance Associative SupDouble

instance Commutative SupDouble
instance Commutative InfDouble

instance Idempotent SupDouble
instance Idempotent InfDouble

instance Homomorphic SupDouble InfDouble where hom (SupDouble a) = InfDouble (-a)
instance Homomorphic InfDouble SupDouble where hom (InfDouble a) = SupDouble (-a)

instance Isomorphic SupDouble InfDouble where iso = (hom, hom)
instance Isomorphic InfDouble SupDouble where iso = (hom, hom)

instance Semilattice SupDouble
instance Semilattice InfDouble

instance Lattice Double where
    type Inf Double = InfDouble
    type Sup Double = SupDouble


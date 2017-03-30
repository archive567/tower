{-# LANGUAGE MultiParamTypeClasses #-}

module Tower.Bool
  (
    Bool(..)
  , And(..)
  , Or(..)
  , Xor(..)
  ) where

import Protolude (Bool(..))
import qualified Protolude as P
import Tower.Ordering
import Tower.Magma

newtype And = And Bool
newtype Or  = Or  Bool
newtype Xor = Xor Bool

instance Magma And where
  And a ⊕ And b = And (a P.&& b)

instance Magma Or  where
  Or  a ⊕ Or  b = Or  (a P.|| b)

instance Magma Xor where
  Xor True ⊕ Xor True = Xor False
  Xor False ⊕ Xor False = Xor False
  _ ⊕ _ = Xor True

instance Associative And
instance Associative Or
instance Associative Xor

instance Commutative And
instance Commutative Or
instance Commutative Xor

instance Idempotent Or
instance Idempotent And

instance Unital Or where unit = Or False
instance Unital And where unit = And True
instance Unital Xor where unit = Xor False

instance Invertible Xor where inv a = a

instance Homomorphic Or And where hom (Or x) = And (P.not x)

instance Homomorphic And Or where hom (And x) = Or (P.not x)

instance Isomorphic And Or where iso = (hom, hom)
instance Isomorphic Or And where iso = (hom, hom)

instance Semilattice And
instance Semilattice Or

instance Lattice Bool where
  type Inf Bool = Or
  type Sup Bool = And

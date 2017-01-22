{-# LANGUAGE PolyKinds #-}

-- | Algebra

module Tower.Algebra (
    -- * general group structure
    Magma(..)
  , Unital(..)
  , Associative(..)
  , Commutative(..)
  , Invertible(..)
  , Idempotent(..)
  , Homomorphic(..)
  , Monoidal(..)
  , Group(..)
    -- ** Additive Structure
  , AdditiveMagma(..)
  , AdditiveUnital(..)
  , AdditiveAssociative(..)
  , AdditiveCommutative(..)
  , AdditiveInvertible(..)
  , AdditiveHomomorphic(..)
  , AdditiveMonoidal(..)
  , Additive(..)
  , AdditiveGroup(..)
    -- ** Multiplicative Structure
  , MultiplicativeMagma(..)
  , MultiplicativeUnital(..)
  , MultiplicativeAssociative(..)
  , MultiplicativeCommutative(..)
  , MultiplicativeInvertible(..)
  , MultiplicativeHomomorphic(..)
  , MultiplicativeMonoidal(..)
  , Multiplicative(..)
  , MultiplicativeGroup(..)
    -- * Distributive
  , Distributive(..)
    -- * Ring
  , Semiring(..)
  , Ring(..)
    -- * Module
  , AdditiveBasis(..)
  , AdditiveGroupBasis(..)
  , AdditiveModule(..)
  , AdditiveGroupModule(..)
  , MultiplicativeBasis(..)
  , MultiplicativeGroupBasis(..)
  , MultiplicativeModule(..)
  , MultiplicativeGroupModule(..)
    -- * Integral
  , Integral(..)
    -- * Metric
  , Metric(..)
  , Normed(..)
  , abs
  , Banach(..)
  , BoundedField(..)
  , infinity
    -- * Exponential
  , ExpRing(..)
  , (^)
  , ExpField(..)
    -- * Tensor Algebra
  , Hilbert(..)
  , TensorAlgebra(..)
  , squaredInnerProductNorm
  , innerProductNorm
  , innerProductDistance
  ) where

import qualified Protolude as P
import Protolude (Double, Float, Int)

-- * Magma structure
-- | A <https://en.wikipedia.org/wiki/Magma_(algebra) Magma> is a tuple (T,⊕) consisting of
--
-- - a type a, and
--
-- - a function (⊕) :: T -> T -> T
--
-- The mathematical laws for a magma are:
--
-- - ⊕ is defined for all possible pairs of type T, and
--
-- - ⊕ is closed in the set of all possible values of type T
--
-- or, more tersly,
--
-- > ∀ a, b ∈ T: a ⊕ b ∈ T
--
-- These laws are true by construction in haskell: the type signature of 'magma' and the above mathematical laws are synonyms.
--
class Magma a where (⊕) :: a -> a -> a

-- | A Unital Magma
--
-- > unit ⊕ a = a
-- > a ⊕ unit = a
--
class Magma a => Unital a where unit :: a

-- | Associative
-- 
-- > (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
class Magma a => Associative a

-- | Commutative
--
-- > a ⊕ b = b ⊕ a
class Magma a => Commutative a

-- | Invertible
--
-- > ∀ a ∈ T: inv a ∈ T
--
-- law is true by construction in Haskell
--
class Magma a => Invertible a where inv :: a -> a

-- | Idempotent
--
-- > a ⊕ a = a
class Magma a => Idempotent a

-- | Homomorphic
--
-- > ∀ a ∈ A: hom a ∈ B
--
-- law is true by construction in Haskell
--
class ( Magma a
      , Magma b) =>
      Homomorphic a b where hom :: a -> b

instance Magma a => Homomorphic a a where hom a = a

-- | A Monoidal Magma is associative and unital.
class ( Associative a
      , Unital a) =>
      Monoidal a

-- | A Group is associative, unital and invertible
class ( Associative a
      , Unital a
      , Invertible a) =>
      Group a

-- * Additive structure
-- The Magma structures are repeated for an additive and multiplicative heirarchy, mostly so we can name the specific operators in the usual ways.
--
-- | 'plus' is used for the additive magma to distinguish from '+' which, by convention, implies commutativity
class AdditiveMagma a where plus :: a -> a -> a

instance AdditiveMagma Double where plus = (P.+)
instance AdditiveMagma Float where plus = (P.+)
instance AdditiveMagma Int where plus = (P.+)

-- | AdditiveUnital
--
-- > zero `plus` a == a
-- > a `plus` zero == a
class AdditiveMagma a => AdditiveUnital a where zero :: a

instance AdditiveUnital Double where zero = 0
instance AdditiveUnital Float where zero = 0
instance AdditiveUnital Int where zero = 0

-- | AdditiveAssociative
--
-- > (a `plus` b) `plus` c == a `plus` (b `plus` c)
class AdditiveMagma a => AdditiveAssociative a

instance AdditiveAssociative Double
instance AdditiveAssociative Float
instance AdditiveAssociative Int

-- | AdditiveCommutative
--
-- > a `plus` b == b `plus` a
class AdditiveMagma a => AdditiveCommutative a

instance AdditiveCommutative Double
instance AdditiveCommutative Float
instance AdditiveCommutative Int

-- | AdditiveInvertible
--
-- > ∀ a ∈ A: negate a ∈ A
--
-- law is true by construction in Haskell
class AdditiveMagma a => AdditiveInvertible a where negate :: a -> a

instance AdditiveInvertible Double where negate = P.negate
instance AdditiveInvertible Float where negate = P.negate
instance AdditiveInvertible Int where negate = P.negate

-- | AdditiveHomomorphic
--
-- > ∀ a ∈ A: plushom a ∈ B
--
-- law is true by construction in Haskell
class ( AdditiveMagma a
      , AdditiveMagma b) =>
      AdditiveHomomorphic a b where
    plushom :: a -> b

instance AdditiveMagma a => AdditiveHomomorphic a a where plushom a = a

-- | AdditiveMonoidal
class ( AdditiveUnital a
      , AdditiveAssociative a) =>
      AdditiveMonoidal a

-- | Additive
--
-- > a + b = b + a
--
-- > (a + b) + c = a + (b + c)
--
-- > zero + a = a
--
-- > a + zero = a
--
class ( AdditiveCommutative a
      , AdditiveUnital a
      , AdditiveAssociative a) =>
      Additive a where
    infixr 6 +
    (+) :: a -> a -> a
    a + b = plus a b

instance Additive Double
instance Additive Float
instance Additive Int

-- | AdditiveGroup
--
-- > a - a = zero
--
-- > negate a = zero - a
--
-- > negate a + a = zero
--
class ( Additive a
      , AdditiveInvertible a) =>
      AdditiveGroup a where
    infixr 6 -
    (-) :: a -> a -> a
    (-) a b = a `plus` negate b

instance AdditiveGroup Double
instance AdditiveGroup Float
instance AdditiveGroup Int

-- * Multiplicative structure
-- | 'times' is used for the multiplicative magma to distinguish from '*' which, by convention, implies commutativity
class MultiplicativeMagma a where times :: a -> a -> a

instance MultiplicativeMagma Double where times = (P.*)
instance MultiplicativeMagma Float where times = (P.*)
instance MultiplicativeMagma Int where times = (P.*)

-- | MultiplicativeUnital
--
-- > one `times` a == a
-- > a `times` one == a
class MultiplicativeMagma a => MultiplicativeUnital a where one :: a

instance MultiplicativeUnital Double where one = 1
instance MultiplicativeUnital Float where one = 1
instance MultiplicativeUnital Int where one = 1

-- | MultiplicativeCommutative
--
-- > a `times` b == b `times` a
class MultiplicativeMagma a => MultiplicativeCommutative a

instance MultiplicativeCommutative Double
instance MultiplicativeCommutative Float
instance MultiplicativeCommutative Int

-- | MultiplicativeAssociative
--
-- > (a `times` b) `times` c == a `times` (b `times` c)
class MultiplicativeMagma a => MultiplicativeAssociative a

instance MultiplicativeAssociative Double
instance MultiplicativeAssociative Float
instance MultiplicativeAssociative Int

-- | MultiplicativeInvertible
--
-- > ∀ a ∈ A: recip a ∈ A
--
-- law is true by construction in Haskell
class MultiplicativeMagma a => MultiplicativeInvertible a where recip :: a -> a

instance MultiplicativeInvertible Double where recip = P.recip
instance MultiplicativeInvertible Float where recip = P.recip

-- | MultiplicativeHomomorphic
--
-- > ∀ a ∈ A: timeshom a ∈ B
--
-- law is true by construction in Haskell
class ( MultiplicativeMagma a
      , MultiplicativeMagma b) =>
      MultiplicativeHomomorphic a b where
    timeshom :: a -> b

instance MultiplicativeMagma a => MultiplicativeHomomorphic a a where
    timeshom a = a

-- | MultiplicativeMonoidal
class ( MultiplicativeUnital a
      , MultiplicativeAssociative a) =>
      MultiplicativeMonoidal a

-- | Multiplicative
--
-- > a * b = b * a
--
-- > (a * b) * c = a * (b * c)
--
-- > one * a = a
--
-- > a * one = a
--
class ( MultiplicativeCommutative a
      , MultiplicativeUnital a
      , MultiplicativeAssociative a) =>
      Multiplicative a where
    infixr 7 *
    (*) :: a -> a -> a
    a * b = times a b

instance Multiplicative Double
instance Multiplicative Float
instance Multiplicative Int

-- | MultiplicativeGroup
--
-- > a / a = one
--
-- > recip a = one / a
--
-- > recip a * a = one
--
class ( Multiplicative a
      , MultiplicativeInvertible a) =>
      MultiplicativeGroup a where
    infixr 7 /
    (/) :: a -> a -> a
    (/) a b = a `times` recip b

instance MultiplicativeGroup Double
instance MultiplicativeGroup Float

-- | Distributive
--
-- > a * zero = zero
--
-- > a * (b + c) == a * b + a * c
--
class (
    Additive a
  , Multiplicative a
  ) => Distributive a

instance Distributive Double
instance Distributive Float
instance Distributive Int

-- | a semiring doesn't need to be multiplicative commutative, so distributive doesn't quite fit as a parent
class ( Additive a
      , MultiplicativeAssociative a
      , MultiplicativeUnital a) =>
      Semiring a

instance Semiring Double
instance Semiring Float
instance Semiring Int

-- | Ring
class ( Additive a
      , AdditiveGroup a
      , Multiplicative a
      , MultiplicativeGroup a) =>
      Ring a

instance Ring Double
instance Ring Float

-- * Additive Module Structure

-- | AdditiveBasis
-- element by element addition
class ( Additive a
      , AdditiveHomomorphic a a) =>
      AdditiveBasis a where
    infixr 7 .+.
    (.+.) :: a -> a -> a
    a .+. b = plushom a + plushom b

-- | AdditiveGroupBasis
-- element by element subtraction
class ( AdditiveGroup a
      , AdditiveHomomorphic a a) =>
      AdditiveGroupBasis a where
    infixr 7 .-.
    (.-.) :: a -> a -> a
    a .-. b = plushom a - plushom b

-- | AdditiveModule
class ( Additive a
      , Additive s
      , AdditiveHomomorphic s a) =>
      AdditiveModule s a where
    infixr 7 .+
    (.+) :: AdditiveModule s a => s -> a -> a
    s .+ a = plushom s + a

    infixr 7 +.
    (+.) :: AdditiveModule s a => a -> s -> a
    a +. s = a + plushom s

-- | AdditiveGroupModule
class ( AdditiveModule s a
      , AdditiveGroup a) =>
      AdditiveGroupModule s a where
    infixr 7 .-
    (.-) :: AdditiveModule s a => s -> a -> a
    s .- a = plushom s + a

    infixr 7 -.
    (-.) :: AdditiveModule s a => a -> s -> a
    a -. s = a - plushom s

-- * Multiplicative Module Structure

-- | MultiplicativeBasis
-- element by element addition
class ( Multiplicative a
      , MultiplicativeHomomorphic a a) =>
      MultiplicativeBasis a where
    infixr 7 .*.
    (.*.) :: a -> a -> a
    a .*. b = timeshom a * timeshom b

-- | MultiplicativeGroupBasis
-- element by element subtraction
class ( MultiplicativeGroup a
      , MultiplicativeHomomorphic a a) =>
      MultiplicativeGroupBasis a where
    infixr 7 ./.
    (./.) :: a -> a -> a
    a ./. b = timeshom a / timeshom b

-- | MultiplicativeModule
class ( Multiplicative a
      , Multiplicative s
      , MultiplicativeHomomorphic s a) =>
      MultiplicativeModule s a where
    infixr 7 .*
    (.*) :: MultiplicativeModule s a => s -> a -> a
    s .* a = timeshom s * a

    infixr 7 *.
    (*.) :: MultiplicativeModule s a => a -> s -> a
    a *. s = a * timeshom s

-- | MultiplicativeGroupModule
class ( MultiplicativeModule s a
      , MultiplicativeGroup a) =>
      MultiplicativeGroupModule s a where
    infixr 7 ./
    (./) :: MultiplicativeModule s a => s -> a -> a
    s ./ a = timeshom s * a

    infixr 7 /.
    (/.) :: MultiplicativeModule s a => a -> s -> a
    a /. s = a / timeshom s

-- | Integral
--
-- > b == zero || b * (a `div` b) + (a `mod` b) == a
-- > b == zero || b * (a `quot` b) + (a `rem` b) == a
--
class (Additive a, Multiplicative a) => Integral a where

    toInteger :: a -> P.Integer

    infixl 7  `quot`, `rem`

    -- | truncates towards zero
    quot :: a -> a -> a
    quot a1 a2 = P.fst (quotRem a1 a2)

    rem :: a -> a -> a
    rem a1 a2 = P.snd (quotRem a1 a2)

    quotRem :: a -> a -> (a,a)

    infixl 7 `div`, `mod`

    -- | truncates towards negative infinity
    div :: a -> a -> a
    div a1 a2 = P.fst (divMod a1 a2)
    mod :: a -> a -> a
    mod a1 a2 = P.snd (divMod a1 a2)

    divMod :: a -> a -> (a,a)

instance Integral Int where
    toInteger = P.toInteger
    quotRem = P.quotRem
    divMod = P.divMod

-- | Metric
class Metric r m where
    d :: m -> m -> r

-- | Normed
class Normed a where
    size :: a -> a

-- | abs
abs :: Normed a => a -> a
abs = size

-- | Banach
class ( MultiplicativeGroup a
      , MultiplicativeModule a a
      , MultiplicativeGroupModule a a
      , MultiplicativeInvertible a
      , Normed a) =>
      Banach a where
    normalize :: a -> a
    normalize a = a ./ size a

-- | BoundedField
class ( AdditiveUnital a
      , MultiplicativeGroup a
      , P.Bounded a) =>
      BoundedField a where
    nan :: a
    nan = zero/zero

instance P.Bounded Float where
    maxBound = one/zero
    minBound = negate (one/zero)
instance BoundedField Float

instance P.Bounded Double where
    maxBound = one/zero
    minBound = negate (one/zero)
instance BoundedField Double

-- | infinity
infinity :: BoundedField a => a
infinity = P.maxBound

-- | ExpRing
class Ring a => ExpRing a where
    logBase :: a -> a -> a

    (**) :: ExpRing a => a -> a -> a
    (**) = P.undefined

-- | (^)
(^) :: ExpRing a => a -> a -> a
(^) = (**)

-- | ExpField
class ( Additive a
      , ExpRing a
      , MultiplicativeGroup a) =>
      ExpField a where
    sqrt :: a -> a
    sqrt a = a**(one/one+one)

    exp :: a -> a
    log :: a -> a

-- | ><
infixr 8 ><
type family (><) (a::k1) (b::k2) :: *

-- | TensorAlgebra
class TensorAlgebra a where
    (><) :: a -> a -> (a><a)
    timesleft :: a -> (a><a) -> a
    timesright :: (a><a) -> a -> a

-- | Hilbert
class (Banach a, TensorAlgebra a, ExpField r, AdditiveGroup a) => Hilbert a r where
    infix 8 <?>
    (<?>) :: a -> a -> r

-- | squaredInnerProductNorm 
squaredInnerProductNorm :: Hilbert v r => v -> r
squaredInnerProductNorm v = v <?> v

-- | innerProductNorm 
innerProductNorm :: (Hilbert v r) => v -> r
innerProductNorm = sqrt P.. squaredInnerProductNorm

-- | innerProductDistance
innerProductDistance :: Hilbert v r => v -> v -> r
innerProductDistance v1 v2 = innerProductNorm (v1 - v2)

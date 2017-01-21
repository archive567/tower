{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

module Tower (
    -- * Magma structure
    Magma(..)
  , Unital(..)
  , Associative(..)
  , Commutative(..)
  , Invertible(..)
  , Idempotent(..)
  , Homomorphic(..)
  , Semigroup(..)
  , Monoid(..)
  , Group(..)
    -- * Additive Structure
  , AdditiveMagma(..)
  , AdditiveUnital(..)
  , AdditiveAssociative(..)
  , AdditiveCommutative(..)
  , AdditiveInvertible(..)
  , AdditiveIdempotent(..)
  , AdditiveHomomorphic(..)
  , AdditiveSemigroup(..)
  , AdditiveMonoid(..)
  , AdditiveGroup(..)
  , (+)
  , (-)
    -- * Multiplicative Structure
  , MultiplicativeMagma(..)
  , MultiplicativeUnital(..)
  , MultiplicativeAssociative(..)
  , MultiplicativeCommutative(..)
  , MultiplicativeInvertible(..)
  , MultiplicativeIdempotent(..)
  , MultiplicativeHomomorphic(..)
  , MultiplicativeSemigroup(..)
  , MultiplicativeMonoid(..)
  , Field(..)
  , (*)
  , (/)
    -- * Distributive
  , Distributive(..)
    -- * Ring
  , Semiring(..)
  , Ring(..)
    -- * Module Structure
  , AdditiveModule(..)
  , (.+)
  , (.-)
  , (.+.)
  , (.-.)
  , MultiplicativeModule(..)
  , (.*)
  , (./)
  , (.*.)
  , (./.)
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
import Protolude (
    Int,
    Integer,
    Float,
    Double,
    Rational,
    Eq(..),
    Bool(..),
    Ord(..),
    Bounded(..),
    (.), ($), undefined, Maybe(..))

-- * Magma structure

-- ![](other/tower-dot.svg)


-- | Magma
-- <https://en.wikipedia.org/wiki/Magma_(algebra)>
-- 
class Magma a where magma :: a -> a -> a
class Magma a => Unital a where unit :: a
class Magma a => Associative a
class Magma a => Commutative a
class Magma a => Invertible a where inv :: a -> a
class Magma a => Idempotent a
class (Magma a, Magma b) => Homomorphic a b where hom :: a -> b
instance Magma a => Homomorphic a a where hom a = a
type Semigroup a = Associative a
type Monoid a = (Semigroup a, Unital a)
type Group a = (Monoid a, Invertible a)

-- | Additive structure
class AdditiveMagma a where plus :: a -> a -> a
class AdditiveMagma a => AdditiveUnital a where zero :: a
class AdditiveMagma a => AdditiveAssociative a
class AdditiveMagma a => AdditiveCommutative a
class AdditiveMagma a => AdditiveInvertible a where negate :: a -> a
class AdditiveMagma a => AdditiveIdempotent a
class (AdditiveMagma a, AdditiveMagma b) => AdditiveHomomorphic a b where
    plushom :: a -> b
instance AdditiveMagma a => AdditiveHomomorphic a a where plushom a = a
type AdditiveSemigroup a = AdditiveAssociative a
type AdditiveMonoid a = (AdditiveUnital a, AdditiveSemigroup a)
class (AdditiveCommutative a, AdditiveMonoid a) => Additive a
type AdditiveGroup a = (Additive a, AdditiveInvertible a)

infixr 6 +
(+) :: Additive a => a -> a -> a
a + b = plus a b

infixr 6 -
(-) :: (AdditiveGroup a) => a -> a -> a
(-) a b = a `plus` negate b

-- | Multiplicative structure
class MultiplicativeMagma a where times :: a -> a -> a
class MultiplicativeMagma a => MultiplicativeIdempotent a
class MultiplicativeMagma a => MultiplicativeCommutative a
class MultiplicativeMagma a => MultiplicativeAssociative a
class MultiplicativeMagma a => MultiplicativeUnital a where one :: a
class MultiplicativeMagma a => MultiplicativeInvertible a where recip :: a -> a
class (MultiplicativeMagma a, MultiplicativeMagma b) =>
    MultiplicativeHomomorphic a b where
    timeshom :: a -> b
instance MultiplicativeMagma a => MultiplicativeHomomorphic a a where
    timeshom a = a
type MultiplicativeSemigroup a = MultiplicativeAssociative a
type MultiplicativeMonoid a = (MultiplicativeUnital a, MultiplicativeSemigroup a)
class (MultiplicativeCommutative a, MultiplicativeMonoid a) => Multiplicative a
type Field a = (Multiplicative a, MultiplicativeInvertible a)

infixr 7 *
(*) :: Multiplicative a => a -> a -> a
a * b = times a b

infixr 7 /
(/) :: (Field a) => a -> a -> a
(/) a b = a `times` recip b

class (
    Additive a
  , Multiplicative a
  ) => Distributive a

-- a semiring doesn't need to be multiplicative commutative, so distributive doesn't quite fit as a parent
class (Additive a, MultiplicativeMonoid a) => Semiring a

class (Additive a, AdditiveGroup a, Multiplicative a, Field a) => Ring a

class (
    Additive a
  , Additive s
  , AdditiveHomomorphic s a
  ) => AdditiveModule s a

infixr 7 .+
(.+) :: AdditiveModule s a => s -> a -> a
s .+ a = plushom s + a

infixr 7 .-
(.-) :: (AdditiveModule s a, AdditiveGroup a) => s -> a -> a
s .- a = plushom s - a

infixr 7 .+.
(.+.) :: (AdditiveMagma a) => a -> a -> a
a .+. b = plushom a `plus` plushom b

infixr 7 .-.
(.-.) :: (AdditiveInvertible a) => a -> a -> a
a .-. b = plushom a `plus` negate (plushom b)

class (
    Multiplicative s
  , Multiplicative a
  , MultiplicativeHomomorphic s a
  ) => MultiplicativeModule s a

infixr 7 .*
(.*) :: MultiplicativeModule s a => s -> a -> a
s .* a = timeshom s * a

infixr 7 ./
(./) :: (MultiplicativeModule s a, Field a) => s -> a -> a
s ./ a = timeshom s `times` recip a

infixr 7 .*.
(.*.) :: MultiplicativeMagma a => a -> a -> a
a .*. b = timeshom a `times` timeshom b

infixr 7 ./.
(./.) :: (MultiplicativeInvertible a) => a -> a -> a
a ./. b = timeshom a `times` recip (timeshom b)

class (Additive a, Multiplicative a) => Integral a where

    toInteger :: a -> Integer

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

class Metric r m where
    d :: m -> m -> r

class Normed a where
    size :: a -> a

abs :: Normed a => a -> a
abs = size

class (MultiplicativeModule a a, MultiplicativeInvertible a, Normed a) => Banach a where
    normalize :: a -> a
    normalize a = a ./ size a

class (AdditiveUnital a, Field a, Bounded a) => BoundedField a where
    nan :: a
    nan = zero/zero

infinity :: BoundedField a => a
infinity = maxBound

class Ring a => ExpRing a where
    logBase :: a -> a -> a

    (**) :: ExpRing a => a -> a -> a
    (**) = undefined

(^) :: ExpRing a => a -> a -> a
(^) = (**)

class (Additive a, ExpRing a, Field a) => ExpField a where
    sqrt :: a -> a
    sqrt a = a**(one/one+one)

    exp :: a -> a
    log :: a -> a

infixr 8 ><
type family (><) (a::k1) (b::k2) :: *

class TensorAlgebra a where
    (><) :: a -> a -> (a><a)
    timesleft :: a -> (a><a) -> a
    timesright :: (a><a) -> a -> a

class (Banach a, TensorAlgebra a, ExpField r, Additive a, AdditiveInvertible a) => Hilbert a r where
    infix 8 <?>
    (<?>) :: a -> a -> r

squaredInnerProductNorm :: Hilbert v r => v -> r
squaredInnerProductNorm v = v<?>v

innerProductNorm :: (Hilbert v r) => v -> r
innerProductNorm = sqrt . squaredInnerProductNorm

innerProductDistance :: Hilbert v r => v -> v -> r
innerProductDistance v1 v2 = innerProductNorm $ v1 - v2



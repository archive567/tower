{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | Algebra tower for Num's

module Tower.Algebra.Num (
    -- * general group structure
    Magma(..)
  , Unital(..)
  , Associative(..)
  , Commutative(..)
  , Invertible(..)
  , Idempotent(..)
  , Homomorphic(..)
  , Monoidal(..)
  , CMonoidal(..)
  , Loop(..)
  , Group(..)
  , Abelian(..)
    -- ** Additive Structure
  , AdditiveMagma(..)
  , AdditiveUnital(..)
  , AdditiveAssociative(..)
  , AdditiveCommutative(..)
  , AdditiveInvertible(..)
  , AdditiveHomomorphic(..)
  , AdditiveIdempotent(..)
  , AdditiveMonoidal(..)
  , Additive(..)
  , AdditiveRightCancellative(..)
  , AdditiveLeftCancellative(..)
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
  , MultiplicativeRightCancellative(..)
  , MultiplicativeLeftCancellative(..)
  , MultiplicativeGroup(..)
    -- * Distributive
  , Distributive(..)
    -- * Ring
  , Semiring(..)
  , Ring(..)
  , CRing(..)
  , Field(..)
  , fromIntegral
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
  , QuotientField(..)
    -- * Metric
  , Metric(..)
  , Normed(..)
  , abs
  , Banach(..)
  , BoundedField(..)
  , infinity
  , neginfinity
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
import Protolude (Double, Float, Int, Integer, Applicative(..), Functor(..), ($), (.))

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

-- | An Associative Magma
-- 
-- > (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
class Magma a => Associative a

-- | A Commutative Magma
--
-- > a ⊕ b = b ⊕ a
class Magma a => Commutative a

-- | An Invertible Magma
--
-- > ∀ a ∈ T: inv a ∈ T
--
-- law is true by construction in Haskell
--
class Magma a => Invertible a where inv :: a -> a

-- | An Idempotent Magma
--
-- > a ⊕ a = a
class Magma a => Idempotent a

-- | A Homomorph between two Magmas
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

-- | A CMonoidal Magma is commutative, associative and unital.
class ( Commutative a
      , Associative a
      , Unital a) =>
      CMonoidal a

-- | A Loop is unital and invertible
class ( Unital a
      , Invertible a) =>
      Loop a

-- | A Group is associative, unital and invertible
class ( Associative a
      , Unital a
      , Invertible a) =>
      Group a

-- | see http://chris-taylor.github.io/blog/2013/02/25/xor-trick/
groupSwap :: (Group a) => (a,a) -> (a,a)
groupSwap (a,b) =
    let a' = a ⊕ b
        b' = a ⊕ inv b
        a'' = inv b' ⊕ a'
    in (a'',b')

-- | An Abelian Group is associative, unital, invertible and commutative
class ( Associative a
      , Unital a
      , Invertible a
      , Commutative a) =>
      Abelian a

-- * Additive structure
-- The Magma structures are repeated for an additive and multiplicative heirarchy, mostly so we can name the specific operators in the usual ways.
--
-- | 'plus' is used for the additive magma to distinguish from '+' which, by convention, implies commutativity
class AdditiveMagma a where plus :: a -> a -> a

instance (P.Num a) => AdditiveMagma a where plus = (P.+)

-- | AdditiveUnital
--
-- > zero `plus` a == a
-- > a `plus` zero == a
class AdditiveMagma a => AdditiveUnital a where zero :: a

instance (P.Num a) => AdditiveUnital a where zero = 0

-- | AdditiveAssociative
--
-- > (a `plus` b) `plus` c == a `plus` (b `plus` c)
class AdditiveMagma a => AdditiveAssociative a

instance (P.Num a) => AdditiveAssociative a

-- | AdditiveCommutative
--
-- > a `plus` b == b `plus` a
class AdditiveMagma a => AdditiveCommutative a

instance (P.Num a) => AdditiveCommutative a

-- | AdditiveInvertible
--
-- > ∀ a ∈ A: negate a ∈ A
--
-- law is true by construction in Haskell
class AdditiveMagma a => AdditiveInvertible a where negate :: a -> a

instance (P.Num a) => AdditiveInvertible a where negate = P.negate

-- | AdditiveHomomorphic
--
-- > ∀ a ∈ A: plushom a ∈ B
--
-- law is true by construction in Haskell
class (AdditiveMagma b) => AdditiveHomomorphic a b where
    plushom :: a -> b

instance AdditiveMagma a => AdditiveHomomorphic a a where plushom a = a

-- | AdditiveIdempotent
--
-- > a `plus` a == a
class AdditiveMagma a => AdditiveIdempotent a

-- | AdditiveMonoidal
class ( AdditiveUnital a
      , AdditiveAssociative a) =>
      AdditiveMonoidal a

-- | Additive is commutative, unital and associative under addition
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
    infixl 6 +
    (+) :: a -> a -> a
    a + b = plus a b

instance (P.Num a) => Additive a

class ( AdditiveUnital a
      , AdditiveAssociative a
      , AdditiveInvertible a) =>
      AdditiveLeftCancellative a where
    infixl 6 ~-
    (~-) :: a -> a -> a
    (~-) a b = negate b `plus` a

class ( AdditiveUnital a
      , AdditiveAssociative a
      , AdditiveInvertible a) =>
      AdditiveRightCancellative a where
    infixl 6 -~
    (-~) :: a -> a -> a
    (-~) a b = a `plus` negate b

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
    infixl 6 -
    (-) :: a -> a -> a
    (-) a b = a `plus` negate b

instance (P.Num a) => AdditiveGroup a

class (AdditiveGroup a) => Epsilon a where eps :: a

instance Epsilon Double where eps = 1e-8
instance Epsilon Float where eps = 1e-8
instance Epsilon Int where eps = zero
instance Epsilon Integer where eps = zero

-- * Multiplicative structure
-- | 'times' is used for the multiplicative magma to distinguish from '*' which, by convention, implies commutativity
class MultiplicativeMagma a where times :: a -> a -> a

instance (P.Num a) => MultiplicativeMagma a where times = (P.*)

-- | MultiplicativeUnital
--
-- > one `times` a == a
-- > a `times` one == a
class MultiplicativeMagma a => MultiplicativeUnital a where one :: a

instance (P.Num a) => MultiplicativeUnital a where one = 1

-- | MultiplicativeCommutative
--
-- > a `times` b == b `times` a
class MultiplicativeMagma a => MultiplicativeCommutative a

instance (P.Num a) => MultiplicativeCommutative a

-- | MultiplicativeAssociative
--
-- > (a `times` b) `times` c == a `times` (b `times` c)
class MultiplicativeMagma a => MultiplicativeAssociative a

instance (P.Num a) => MultiplicativeAssociative a

-- | MultiplicativeInvertible
--
-- > ∀ a ∈ A: recip a ∈ A
--
-- law is true by construction in Haskell
class MultiplicativeMagma a => MultiplicativeInvertible a where recip :: a -> a

instance (P.Fractional a) => MultiplicativeInvertible a where recip = P.recip

-- | MultiplicativeHomomorphic
--
-- > ∀ a ∈ A: timeshom a ∈ B
--
-- law is true by construction in Haskell
class ( MultiplicativeMagma b) =>
      MultiplicativeHomomorphic a b where
    timeshom :: a -> b

instance MultiplicativeMagma a => MultiplicativeHomomorphic a a where
    timeshom a = a

-- | MultiplicativeMonoidal
class ( MultiplicativeUnital a
      , MultiplicativeAssociative a) =>
      MultiplicativeMonoidal a

-- | Multiplicative is commutative, associative and unital under multiplication
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
    infixl 7 *
    (*) :: a -> a -> a
    a * b = times a b

instance (P.Num a) => Multiplicative a

class ( MultiplicativeUnital a
      , MultiplicativeAssociative a
      , MultiplicativeInvertible a) =>
      MultiplicativeLeftCancellative a where
    infixl 7 ~/
    (~/) :: a -> a -> a
    a ~/ b = recip b `times` a

class ( MultiplicativeUnital a
      , MultiplicativeAssociative a
      , MultiplicativeInvertible a) =>
      MultiplicativeRightCancellative a where
    infixl 7 /~
    (/~) :: a -> a -> a
    a /~ b = a `times` recip b

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
    infixl 7 /
    (/) :: a -> a -> a
    (/) a b = a `times` recip b

instance (P.Fractional a) => MultiplicativeGroup a

-- | Distributive
--
-- > a . (b + c) == a . b + a . c
--
-- > (a + b) . c == a . c + b . c
--
class (
    Additive a
  , MultiplicativeMagma a
  ) => Distributive a

instance (P.Num a) => Distributive a

-- | a semiring
class ( Additive a
      , MultiplicativeAssociative a
      , MultiplicativeUnital a
      , Distributive a) =>
      Semiring a

instance (P.Num a) => Semiring a

-- | Ring
class ( AdditiveGroup a
      , MultiplicativeAssociative a
      , MultiplicativeUnital a
      , Distributive a) =>
      Ring a where
    fromInteger :: Integer -> a
    fromInteger = slowFromInteger

slowFromInteger :: (Ring r) => Integer -> r
slowFromInteger i = if i P.> 0
                    then P.foldl' (+) zero $ P.map (P.const one) [1..i]
                    else negate $ P.foldl' (+) zero $ P.map (P.const one) [1..negate i]

instance (P.Num a) => Ring a where fromInteger = P.fromInteger

-- | CRing is a Commutative Ring.  It arises often due to * being defined as only multiplicative commutative, yet fromInteger being a `Integer -> Ring` (and thus not necessarily commutative).
class ( Multiplicative a, Ring a) => CRing a

instance (P.Num a) => CRing a

-- | Field
class ( AdditiveGroup a
      , MultiplicativeGroup a
      , Distributive a
      , Ring a) =>
      Field a

instance (P.Fractional a) => Field a

-- * Additive Module Structure

-- | AdditiveBasis
-- element by element addition
class ( Applicative m
      , Additive a ) =>
      AdditiveBasis m a where
    infixl 7 .+.
    (.+.) :: m a -> m a -> m a
    (.+.) = P.liftA2 (+)

-- | AdditiveGroupBasis
-- element by element subtraction
class ( Applicative m
      , AdditiveGroup a ) =>
      AdditiveGroupBasis m a where
    infixl 6 .-.
    (.-.) :: m a -> m a -> m a
    (.-.) = P.liftA2 (-)

-- | AdditiveModule
class ( Functor m
      , Additive a) =>
      AdditiveModule m a where
    infixl 6 .+
    (.+) :: AdditiveModule m a => m a -> a -> m a
    m .+ a = fmap (a+) m

    infixl 6 +.
    (+.) :: AdditiveModule m a => a -> m a -> m a
    a +. m = fmap (a+) m

-- | AdditiveGroupModule
class ( Functor m
      , AdditiveGroup a) =>
      AdditiveGroupModule m a where
    infixl 6 .-
    (.-) :: AdditiveModule m a => m a -> a -> m a
    m .- a = fmap (\x -> x - a) m

    infixl 6 -.
    (-.) :: AdditiveModule m a => a -> m a -> m a
    a -. m = fmap (\x -> a - x) m

-- * Multiplicative Module Structure

-- | MultiplicativeBasis
-- element by element multiplication
class ( Applicative m
      , Multiplicative a ) =>
      MultiplicativeBasis m a where
    infixl 7 .*.
    (.*.) :: m a -> m a -> m a
    (.*.) = P.liftA2 (*)

-- | MultiplicativeGroupBasis
-- element by element division
class ( Applicative m
      , MultiplicativeGroup a ) =>
      MultiplicativeGroupBasis m a where
    infixl 7 ./.
    (./.) :: m a -> m a -> m a
    (./.) = P.liftA2 (/)

-- | MultiplicativeModule
class ( Functor m
      , Multiplicative a) =>
      MultiplicativeModule m a where
    infixl 7 .*
    (.*) :: MultiplicativeModule m a => m a -> a -> m a
    m .* a = fmap (a*) m

    infixl 7 *.
    (*.) :: MultiplicativeModule m a => a -> m a -> m a
    a *. m = fmap (a*) m

-- | MultiplicativeGroupModule
class ( Functor m
      , MultiplicativeGroup a) =>
      MultiplicativeGroupModule m a where
    infixl 7 ./
    (./) :: MultiplicativeModule m a => m a -> a -> m a
    m ./ a = fmap (/ a) m

    infixl 7 /.
    (/.) :: MultiplicativeModule m a => a -> m a -> m a
    a /. m = fmap (\x -> a / x) m

-- | Integral
--
-- > b == zero || b * (a `div` b) + (a `mod` b) == a
-- > b == zero || b * (a `quot` b) + (a `rem` b) == a
--
class (Ring a) => Integral a where

    toInteger :: a -> P.Integer

    infixl 7 `div`, `mod`

    -- | truncates towards negative infinity
    div :: a -> a -> a
    div a1 a2 = P.fst (divMod a1 a2)
    mod :: a -> a -> a
    mod a1 a2 = P.snd (divMod a1 a2)

    divMod :: a -> a -> (a,a)
 
instance Integral Int where
    toInteger = P.toInteger
    divMod = P.divMod

instance Integral Integer where
    toInteger = P.toInteger
    divMod = P.divMod

fromIntegral :: (Integral a, Ring b) => a -> b
fromIntegral = fromInteger . toInteger

class (Ring r, Integral s) => QuotientField r s where
    truncate :: r -> s
    round :: r -> s
    ceiling :: r -> s
    floor :: r -> s
    (^^) :: r -> s -> r

instance QuotientField Float Int where
    truncate = P.truncate
    round = P.round
    ceiling = P.ceiling
    floor = P.floor
    (^^) = (P.^^)

instance QuotientField Float Integer where
    truncate = P.truncate
    round = P.round
    ceiling = P.ceiling
    floor = P.floor
    (^^) = (P.^^)

instance QuotientField Double Int where
    truncate = P.truncate
    round = P.round
    ceiling = P.ceiling
    floor = P.floor
    (^^) = (P.^^)

instance QuotientField Double Integer where
    truncate = P.truncate
    round = P.round
    ceiling = P.ceiling
    floor = P.floor
    (^^) = (P.^^)

-- | Metric
class Metric r m where
    distance :: m -> m -> r

-- | Normed
class Normed a b where
    size :: a -> b

instance (P.Num a) => Normed a a where size = P.abs

-- | abs
abs :: Normed a b => a -> b
abs = size

-- | Banach
class ( MultiplicativeGroup a
      , MultiplicativeModule m a
      , MultiplicativeGroupModule m a
      , MultiplicativeInvertible a
      , Normed (m a) a) =>
      Banach m a where
    normalize :: m a -> m a
    normalize a = a ./ size a

-- | BoundedField
class ( Field a
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

neginfinity :: BoundedField a => a
neginfinity = P.minBound

-- | ExpRing
class Ring a => ExpRing a where
    logBase :: a -> a -> a
    (**) :: ExpRing a => a -> a -> a

-- | (^)
(^) :: ExpRing a => a -> a -> a
(^) = (**)

instance ExpRing Double where
    logBase = P.logBase
    (**) = (P.**)
instance ExpRing Float where
    logBase = P.logBase
    (**) = (P.**)

-- | ExpField
class ( Field a
      , ExpRing a ) =>
      ExpField a where
    sqrt :: a -> a
    sqrt a = a**(one/one+one)

    exp :: a -> a
    log :: a -> a

instance ExpField Double where
    exp = P.exp
    log = P.log

instance ExpField Float where
    exp = P.exp
    log = P.log

-- | ><
infixl 8 ><
type family (><) (a::k1) (b::k2) :: *

-- | TensorAlgebra
class TensorAlgebra a where
    (><) :: a -> a -> (a><a)
    timesleft :: a -> (a><a) -> a
    timesright :: (a><a) -> a -> a

-- | Hilbert
class (Banach m a, TensorAlgebra a, ExpField r, AdditiveGroup (m a)) => Hilbert m a r where
    infix 8 <?>
    (<?>) :: m a -> m a -> r

-- | squaredInnerProductNorm 
squaredInnerProductNorm :: Hilbert m v r => m v -> r
squaredInnerProductNorm v = v <?> v

-- | innerProductNorm 
innerProductNorm :: (Hilbert m v r) => m v -> r
innerProductNorm = sqrt P.. squaredInnerProductNorm

-- | innerProductDistance
innerProductDistance :: Hilbert m v r => m v -> m v -> r
innerProductDistance v1 v2 = innerProductNorm (v1 - v2)

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra

module Tower.Algebra (
    -- * general group structure
    Magma(..)
  , Unital(..)
  , Associative
  , Commutative
  , Invertible(..)
  , Idempotent
  , Homomorphic(..)
  , Monoidal
  , CMonoidal
  , Loop
  , Group
  , groupSwap
  , Abelian
    -- ** Additive Structure
  , AdditiveMagma(..)
  , AdditiveUnital(..)
  , AdditiveAssociative
  , AdditiveCommutative
  , AdditiveInvertible(..)
  , AdditiveHomomorphic(..)
  , AdditiveIdempotent
  , AdditiveMonoidal
  , Additive(..)
  , AdditiveRightCancellative(..)
  , AdditiveLeftCancellative(..)
  , AdditiveGroup(..)
    -- ** Multiplicative Structure
  , MultiplicativeMagma(..)
  , MultiplicativeUnital(..)
  , MultiplicativeAssociative
  , MultiplicativeCommutative
  , MultiplicativeInvertible(..)
  , MultiplicativeHomomorphic(..)
  , MultiplicativeMonoidal
  , Multiplicative(..)
  , MultiplicativeRightCancellative(..)
  , MultiplicativeLeftCancellative(..)
  , MultiplicativeGroup(..)
    -- * Distribution
  , Distribution
    -- * Ring
  , Semiring
  , Ring
  , CRing
  , Field
    -- * Integral
  , Integral(..)
  , ToInteger(..)
  , FromInteger(..)
  , fromIntegral
  , QuotientField(..)
    -- * Metric
  , Bounded(..)
  , Metric(..)
  , Normed(..)
  , Signed(..)
  , Epsilon(..)
  , (≈)
  , Banach(..)
  , BoundedField(..)
  , infinity
  , neginfinity
    -- * Exponential
  , ExpRing(..)
  , (^)
  , ExpField(..)
    -- * Module
  , AdditiveBasis(..)
  , AdditiveGroupBasis(..)
  , AdditiveModule(..)
  , AdditiveGroupModule(..)
  , MultiplicativeBasis(..)
  , MultiplicativeGroupBasis(..)
  , MultiplicativeModule(..)
  , MultiplicativeGroupModule(..)
    -- * Tensoring
  , Hilbert(..)
  , type (><)
  , TensorProduct(..)
  , E(..)
  ) where

import qualified Protolude as P
import Protolude (Double, Float, Int, Integer, Functor(..), ($), (.), (<$>), Foldable(..), fst, snd, foldr, const, Bool(..), Ord(..), Eq(..), any)
import Data.Functor.Rep
import Data.Distributive

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

instance AdditiveMagma Double where plus = (P.+)
instance AdditiveMagma Float where plus = (P.+)
instance AdditiveMagma Int where plus = (P.+)
instance AdditiveMagma Integer where plus = (P.+)
instance (Representable r, AdditiveMagma a) => AdditiveMagma (r a) where
    plus = liftR2 plus

-- | AdditiveUnital
--
-- > zero `plus` a == a
-- > a `plus` zero == a
class AdditiveMagma a => AdditiveUnital a where zero :: a

instance AdditiveUnital Double where zero = 0
instance AdditiveUnital Float where zero = 0
instance AdditiveUnital Int where zero = 0
instance AdditiveUnital Integer where zero = 0
instance (Representable r, AdditiveUnital a) => AdditiveUnital (r a) where
    zero = pureRep zero

-- | AdditiveAssociative
--
-- > (a `plus` b) `plus` c == a `plus` (b `plus` c)
class AdditiveMagma a => AdditiveAssociative a

instance AdditiveAssociative Double
instance AdditiveAssociative Float
instance AdditiveAssociative Int
instance AdditiveAssociative Integer
instance (Representable r, AdditiveAssociative a) => AdditiveAssociative (r a)

-- | AdditiveCommutative
--
-- > a `plus` b == b `plus` a
class AdditiveMagma a => AdditiveCommutative a

instance AdditiveCommutative Double
instance AdditiveCommutative Float
instance AdditiveCommutative Int
instance AdditiveCommutative Integer
instance (Representable r, AdditiveCommutative a) => AdditiveCommutative (r a)

-- | AdditiveInvertible
--
-- > ∀ a ∈ A: negate a ∈ A
--
-- law is true by construction in Haskell
class AdditiveMagma a => AdditiveInvertible a where negate :: a -> a

instance AdditiveInvertible Double where negate = P.negate
instance AdditiveInvertible Float where negate = P.negate
instance AdditiveInvertible Int where negate = P.negate
instance AdditiveInvertible Integer where negate = P.negate
instance (Representable r, AdditiveInvertible a) => AdditiveInvertible (r a) where
    negate a = fmapRep negate a

-- | AdditiveHomomorphic
--
-- > ∀ a ∈ A: plushom a ∈ B
--
-- law is true by construction in Haskell
class (AdditiveMagma b) => AdditiveHomomorphic a b where
    plushom :: a -> b

instance AdditiveMagma a => AdditiveHomomorphic a a where plushom a = a
instance (Representable r, AdditiveMagma a) => AdditiveHomomorphic a (r a) where
    plushom a = pureRep a

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

instance Additive Double
instance Additive Float
instance Additive Int
instance Additive Integer
instance (Representable r, Additive a) => Additive (r a)

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

instance AdditiveGroup Double
instance AdditiveGroup Float
instance AdditiveGroup Int
instance AdditiveGroup Integer
instance (Representable r, AdditiveGroup a) => AdditiveGroup (r a)

-- * Multiplicative structure
-- | 'times' is used for the multiplicative magma to distinguish from '*' which, by convention, implies commutativity
class MultiplicativeMagma a where times :: a -> a -> a

instance MultiplicativeMagma Double where times = (P.*)
instance MultiplicativeMagma Float where times = (P.*)
instance MultiplicativeMagma Int where times = (P.*)
instance MultiplicativeMagma Integer where times = (P.*)
instance (Representable r, MultiplicativeMagma a) => MultiplicativeMagma (r a) where
    times = liftR2 times

-- | MultiplicativeUnital
--
-- > one `times` a == a
-- > a `times` one == a
class MultiplicativeMagma a => MultiplicativeUnital a where one :: a

instance MultiplicativeUnital Double where one = 1
instance MultiplicativeUnital Float where one = 1
instance MultiplicativeUnital Int where one = 1
instance MultiplicativeUnital Integer where one = 1
instance (Representable r, MultiplicativeUnital a) =>
    MultiplicativeUnital (r a) where
    one = pureRep one

-- | MultiplicativeCommutative
--
-- > a `times` b == b `times` a
class MultiplicativeMagma a => MultiplicativeCommutative a

instance MultiplicativeCommutative Double
instance MultiplicativeCommutative Float
instance MultiplicativeCommutative Int
instance MultiplicativeCommutative Integer
instance (Representable r, MultiplicativeCommutative a) =>
    MultiplicativeCommutative (r a)

-- | MultiplicativeAssociative
--
-- > (a `times` b) `times` c == a `times` (b `times` c)
class MultiplicativeMagma a => MultiplicativeAssociative a

instance MultiplicativeAssociative Double
instance MultiplicativeAssociative Float
instance MultiplicativeAssociative Int
instance MultiplicativeAssociative Integer
instance (Representable r, MultiplicativeAssociative a) =>
    MultiplicativeAssociative (r a)

-- | MultiplicativeInvertible
--
-- > ∀ a ∈ A: recip a ∈ A
--
-- law is true by construction in Haskell
class MultiplicativeMagma a => MultiplicativeInvertible a where recip :: a -> a

instance MultiplicativeInvertible Double where recip = P.recip
instance MultiplicativeInvertible Float where recip = P.recip
instance (Representable r, MultiplicativeInvertible a) =>
    MultiplicativeInvertible (r a) where
    recip = fmapRep recip

-- | MultiplicativeHomomorphic
--
-- > ∀ a ∈ A: timeshom a ∈ B
--
-- law is true by construction in Haskell
class ( MultiplicativeMagma b) =>
      MultiplicativeHomomorphic a b where
    timeshom :: a -> b

instance (Representable r, MultiplicativeMagma a) =>
    MultiplicativeHomomorphic a (r a) where
    timeshom a = pureRep a

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

instance Multiplicative Double
instance Multiplicative Float
instance Multiplicative Int
instance Multiplicative Integer
instance (Representable r, Multiplicative a) => Multiplicative (r a)

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

instance MultiplicativeGroup Double
instance MultiplicativeGroup Float
instance (Representable r, MultiplicativeGroup a) => MultiplicativeGroup (r a)

-- | Distribution
--
-- > a . (b + c) == a . b + a . c
--
-- > (a + b) . c == a . c + b . c
--
class (
    Additive a
  , MultiplicativeMagma a
  ) => Distribution a

instance Distribution Double
instance Distribution Float
instance Distribution Int
instance Distribution Integer
instance (Representable r, Distribution a) => Distribution (r a)

-- | a semiring
class ( Additive a
      , MultiplicativeAssociative a
      , MultiplicativeUnital a
      , Distribution a) =>
      Semiring a

instance Semiring Double
instance Semiring Float
instance Semiring Int
instance Semiring Integer
instance (Representable r, Semiring a) => Semiring (r a)

-- | Ring
class ( AdditiveGroup a
      , MultiplicativeAssociative a
      , MultiplicativeUnital a
      , Distribution a) =>
      Ring a

instance Ring Double
instance Ring Float
instance Ring Int
instance Ring Integer
instance (Representable r, Ring a) => Ring (r a)

-- | CRing is a Commutative Ring.  It arises often due to * being defined as only multiplicative commutative, yet fromInteger being a `Integer -> Ring` (and thus not necessarily commutative).
class ( Multiplicative a, Ring a) => CRing a

instance CRing Double
instance CRing Float
instance CRing Int
instance CRing Integer
instance (Representable r, CRing a) => CRing (r a)

-- | Field
class ( AdditiveGroup a
      , MultiplicativeGroup a
      , Distribution a
      , Ring a) =>
      Field a

instance Field Double
instance Field Float
instance (Representable r, Field a) => Field (r a)

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

class (Field a) => Bounded a where
    maxBound :: a
    maxBound = one/zero
    minBound :: a
    minBound = negate (one/zero)

instance Bounded Float
instance Bounded Double
instance (Field a, Representable r) => Bounded (r a) where
    maxBound = one/zero
    minBound = negate (one/zero)

class ( AdditiveUnital a
      , AdditiveGroup a
      , Multiplicative a
      ) => Signed a where
    sign :: a -> a
    abs :: a -> a

instance Signed Double where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance Signed Float where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance Signed Int where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance Signed Integer where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance (Representable r, Signed a) => Signed (r a) where
    sign = fmapRep sign
    abs = fmapRep abs

-- | Normed
class Normed a b where
    size :: a -> b

instance Normed Double Double where size = P.abs
instance Normed Float Float where size = P.abs
instance Normed Int Int where size = P.abs
instance Normed Integer Integer where size = P.abs
instance (Foldable r, Representable r, ExpField a, ExpRing a) =>
    Normed (r a) a where
    size r = sqrt $ foldr (+) zero $ (**(one+one)) <$> r

-- | Epsilon
class (AdditiveGroup a) => Epsilon a where
    nearZero :: a -> Bool
    aboutEqual :: a -> a -> Bool

infixl 4 ≈

(≈) :: (Epsilon a) => a -> a -> Bool
(≈) = aboutEqual

instance Epsilon Double where
    nearZero a = abs a <= (1e-12 :: Double)
    aboutEqual a b = nearZero $ a - b

instance Epsilon Float where
    nearZero a = abs a <= (1e-6 :: Float)
    aboutEqual a b = nearZero $ a - b

instance Epsilon Int where
    nearZero a = a == zero
    aboutEqual a b = nearZero $ a - b

instance Epsilon Integer where
    nearZero a = a == zero
    aboutEqual a b = nearZero $ a - b

instance (Foldable r, Representable r, Epsilon a) => Epsilon (r a) where
    nearZero a = any nearZero $ toList a
    aboutEqual a b = any P.identity $ liftR2 aboutEqual a b

-- | Metric
class Metric a b where
    distance :: a -> a -> b

instance Metric Double Double where distance a b = abs (a - b)
instance Metric Float Float where distance a b = abs (a - b)
instance Metric Int Int where distance a b = abs (a - b)
instance Metric Integer Integer where distance a b = abs (a - b)

instance (P.Foldable r, Representable r, ExpField a) => Metric (r a) a where
    distance a b = size (a - b)

-- | BoundedField
class ( Field a
      , Bounded a) =>
      BoundedField a where
    nan :: a
    nan = zero/zero

    isNaN :: a -> Bool

infinity :: BoundedField a => a
infinity = maxBound

neginfinity :: BoundedField a => a
neginfinity = minBound

instance BoundedField Float where isNaN = P.isNaN
instance BoundedField Double where isNaN = P.isNaN
instance (Foldable r, Representable r, BoundedField a) =>
    BoundedField (r a) where
    isNaN a = any isNaN a

class (Ring a) => QuotientField a where
    round :: a -> Integer
    ceiling :: a -> Integer
    floor :: a -> Integer
    (^^) :: a -> Integer -> a

instance QuotientField Float where
    round = P.round
    ceiling = P.ceiling
    floor = P.floor
    (^^) = (P.^^)

instance QuotientField Double where
    round = P.round
    ceiling = P.ceiling
    floor = P.floor
    (^^) = (P.^^)

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

-- * Additive Module Structure

-- | AdditiveBasis
-- element by element addition
class ( Representable m
      , Additive a ) =>
      AdditiveBasis m a where
    infixl 7 .+.
    (.+.) :: m a -> m a -> m a
    (.+.) = liftR2 (+)

instance (Representable r, Additive a) => AdditiveBasis r a

-- | AdditiveGroupBasis
-- element by element subtraction
class ( Representable m
      , AdditiveGroup a ) =>
      AdditiveGroupBasis m a where
    infixl 6 .-.
    (.-.) :: m a -> m a -> m a
    (.-.) = liftR2 (-)

instance (Representable r, AdditiveGroup a) => AdditiveGroupBasis r a

-- | AdditiveModule
class ( Representable m
      , Additive a) =>
      AdditiveModule m a where
    infixl 6 .+
    (.+) :: m a -> a -> m a
    m .+ a = fmap (a+) m

    infixl 6 +.
    (+.) :: a -> m a -> m a
    a +. m = fmap (a+) m

instance (Representable r, Additive a) => AdditiveModule r a

-- | AdditiveGroupModule
class ( Representable m
      , AdditiveGroup a) =>
      AdditiveGroupModule m a where
    infixl 6 .-
    (.-) :: m a -> a -> m a
    m .- a = fmap (\x -> x - a) m

    infixl 6 -.
    (-.) :: a -> m a -> m a
    a -. m = fmap (\x -> a - x) m

instance (Representable r, AdditiveGroup a) => AdditiveGroupModule r a

-- * Multiplicative Module Structure

-- | MultiplicativeBasis
-- element by element multiplication
class ( Representable m
      , Multiplicative a ) =>
      MultiplicativeBasis m a where
    infixl 7 .*.
    (.*.) :: m a -> m a -> m a
    (.*.) = liftR2 (*)

instance (Representable r, Multiplicative a) => MultiplicativeBasis r a

-- | MultiplicativeGroupBasis
-- element by element division
class ( Representable m
      , MultiplicativeGroup a ) =>
      MultiplicativeGroupBasis m a where
    infixl 7 ./.
    (./.) :: m a -> m a -> m a
    (./.) = liftR2 (/)

instance (Representable r, MultiplicativeGroup a) => MultiplicativeGroupBasis r a

-- | MultiplicativeModule
class ( Representable m
      , Multiplicative a) =>
      MultiplicativeModule m a where
    infixl 7 .*
    (.*) :: m a -> a -> m a
    m .* a = fmap (a*) m

    infixl 7 *.
    (*.) :: a -> m a -> m a
    a *. m = fmap (a*) m

instance (Representable r, Multiplicative a) => MultiplicativeModule r a

-- | MultiplicativeGroupModule
class ( Representable m
      , MultiplicativeGroup a) =>
      MultiplicativeGroupModule m a where
    infixl 7 ./
    (./) :: m a -> a -> m a
    m ./ a = fmap (/ a) m

    infixl 7 /.
    (/.) :: a -> m a -> m a
    a /. m = fmap (\x -> a / x) m

instance (Representable r, MultiplicativeGroup a) => MultiplicativeGroupModule r a

-- | Banach
class ( MultiplicativeGroup a
      , MultiplicativeModule m a
      , MultiplicativeGroupModule m a
      , MultiplicativeInvertible a
      , Normed (m a) a) =>
      Banach m a where
    normalize :: m a -> m a
    normalize a = a ./ size a

instance (Foldable r, Representable r, ExpField a) => Banach r a

-- | Hilbert
class (AdditiveGroup (m a)) => Hilbert m a where
    infix 8 <.>
    (<.>) :: m a -> m a -> a

instance (Foldable r, Representable r, CRing a) =>
    Hilbert r a where
    (<.>) a b = foldl' (+) zero $ liftR2 (*) a b

-- | tensorial tomfoolery
type family (><) (a::k1) (b::k2) :: *

type instance Int >< Int = Int
type instance Integer >< Integer = Integer
type instance Double >< Double = Double
type instance Float >< Float = Float

type family TensorRep k1 k2 where
    TensorRep (r a) (r a) = r (r a)
    TensorRep (r a) a = r a

type instance r a >< b = TensorRep (r a) b

-- | TensorAlgebra
class TensorProduct a where
    infix 8 ><
    (><) :: a -> a -> (a><a)
    timesleft :: a -> (a><a) -> a
    timesright :: (a><a) -> a -> a

instance (Foldable r, Representable r, CRing a ) =>
    TensorProduct (r a)
  where
    (><) m n = tabulate (\i -> index m i *. n)
    timesleft v m = tabulate (\i -> v <.> index m i)
    timesright m v = tabulate (\i -> v <.> index m i)

newtype E a = E { e :: a } deriving (Functor)

instance Distributive E where
  distribute = E . fmap (\(E x) -> x)

instance Representable E where
  type Rep E = ()
  tabulate f = E (f ())
  index (E x) () = x

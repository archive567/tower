{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Tower where

import Protolude (Bool(..), (.), (==), (/=), (&&), (||), asTypeOf, (>), (<), fst, snd, foldl, const, foldl', numerator, denominator, ($), error)
import qualified Protolude as P

class Semigroup g where
    {-# MINIMAL (+) #-}
    infixl 6 +
    (+) :: g -> g -> g

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.
cycle :: Semigroup m => m -> m
cycle xs = xs' where xs' = xs + xs'

instance Semigroup P.Int      where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup P.Integer  where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup P.Float    where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup P.Double   where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup P.Rational where (+) = (P.+) ; {-# INLINE (+) #-}

instance Semigroup () where
    {-# INLINE (+) #-}
    ()+() = ()

instance Semigroup b => Semigroup (a -> b) where
    {-# INLINE (+) #-}
    f+g = \a -> f a + g a

---------------------------------------

-- | This type class is only used by the "Action" class.
-- It represents the semigroup that acts on our type.
type family Actor s

-- | Semigroup actions let us apply a semigroup to a set.
-- The theory of Modules is essentially the theory of Ring actions.
-- (See <http://mathoverflow.net/questions/100565/why-are-ring-actions-much-harder-to-find-than-group-actions mathoverflow>.)
-- That is why the two classes use similar notation.
--
-- See <https://en.wikipedia.org/wiki/Semigroup_action wikipedia> for more detail.
--
-- FIXME: These types could probably use a more expressive name.
--
-- FIXME: We would like every Semigroup to act on itself, but this results in a class cycle.
class (Semigroup (Actor s)) => Action s where
    {-# MINIMAL (.+) #-}
    infixl 6 .+
    (.+) :: s -> Actor s -> s

-- | > s .+ a = a +. s
{-# INLINE (+.) #-}
infixr 6 +.
(+.) :: Action s => Actor s -> s -> s
a +. s = s .+ a

type instance Actor P.Int      = P.Int
type instance Actor P.Integer  = P.Integer
type instance Actor P.Float    = P.Float
type instance Actor P.Double   = P.Double
type instance Actor P.Rational = P.Rational
type instance Actor ()       = ()
type instance Actor (a->b)   = a->Actor b

instance Action P.Int      where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action P.Integer  where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action P.Float    where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action P.Double   where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action P.Rational where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action ()       where (.+) = (+) ; {-# INLINE (.+) #-}

instance Action b => Action (a->b) where
    {-# INLINE (.+) #-}
    f.+g = \x -> f x.+g x

---------------------------------------

class Semigroup g => Monoid g where
    zero :: g

---------
instance Monoid P.Int       where zero = 0 ; {-# INLINE zero #-}
instance Monoid P.Integer   where zero = 0 ; {-# INLINE zero #-}
instance Monoid P.Float     where zero = 0 ; {-# INLINE zero #-}
instance Monoid P.Double    where zero = 0 ; {-# INLINE zero #-}
instance Monoid P.Rational  where zero = 0 ; {-# INLINE zero #-}

instance Monoid () where
    {-# INLINE zero #-}
    zero = ()

instance Monoid b => Monoid (a -> b) where
    {-# INLINE zero #-}
    zero = \a -> zero

-- | In a cancellative semigroup,
--
-- 1)
--
-- > a + b = a + c   ==>   b = c
-- so
-- > (a + b) - b = a + (b - b) = a
--
-- 2)
--
-- > b + a = c + a   ==>   b = c
-- so
-- > -b + (b + a) = (-b + b) + a = a
--
-- This allows us to define "subtraction" in the semigroup.
-- If the semigroup is embeddable in a group, subtraction can be thought of as performing the group subtraction and projecting the result back into the domain of the cancellative semigroup.
-- It is an open problem to fully characterize which cancellative semigroups can be embedded into groups.
--
-- See <http://en.wikipedia.org/wiki/Cancellative_semigroup wikipedia> for more details.
class Semigroup g => Cancellative g where
    {-# MINIMAL (-) #-}

    infixl 6 -
    (-) :: g -> g -> g

instance Cancellative P.Int        where (-) = (P.-) ; {-# INLINE (-) #-}
instance Cancellative P.Integer    where (-) = (P.-) ; {-# INLINE (-) #-}
instance Cancellative P.Float      where (-) = (P.-) ; {-# INLINE (-) #-}
instance Cancellative P.Double     where (-) = (P.-) ; {-# INLINE (-) #-}
instance Cancellative P.Rational   where (-) = (P.-) ; {-# INLINE (-) #-}

instance Cancellative () where
    {-# INLINE (-) #-}
    ()-() = ()

instance Cancellative b => Cancellative (a -> b) where
    {-# INLINE (-) #-}
    f-g = \a -> f a - g a

---------------------------------------

class (Cancellative g, Monoid g) => Group g where
    {-# INLINE negate #-}
    negate :: g -> g
    negate g = zero - g

instance Group P.Int        where negate = P.negate ; {-# INLINE negate #-}
instance Group P.Integer    where negate = P.negate ; {-# INLINE negate #-}
instance Group P.Float      where negate = P.negate ; {-# INLINE negate #-}
instance Group P.Double     where negate = P.negate ; {-# INLINE negate #-}
instance Group P.Rational   where negate = P.negate ; {-# INLINE negate #-}

instance Group () where
    {-# INLINE negate #-}
    negate () = ()

instance Group b => Group (a -> b) where
    {-# INLINE negate #-}
    negate f = negate . f

---------------------------------------

class Semigroup m => Abelian m

instance Abelian P.Int
instance Abelian P.Integer
instance Abelian P.Float
instance Abelian P.Double
instance Abelian P.Rational

instance Abelian ()

instance Abelian b => Abelian (a -> b)

---------------------------------------

-- | A Rg is a Ring without multiplicative identity or negative numbers.
-- (Hence the removal of the i and n from the name.)
--
-- There is no standard terminology for this structure.
-- They might also be called \"semirings without identity\", \"pre-semirings\", or \"hemirings\".
-- See <http://math.stackexchange.com/questions/359437/name-for-a-semiring-minus-multiplicative-identity-requirement this stackexchange question> for a discussion on naming.
--
class (Abelian r, Monoid r) => Rg r where
    {-# MINIMAL (*) #-}

    infixl 7 *
    (*) :: r -> r -> r

instance Rg P.Int         where (*) = (P.*) ; {-# INLINE (*) #-}
instance Rg P.Integer     where (*) = (P.*) ; {-# INLINE (*) #-}
instance Rg P.Float       where (*) = (P.*) ; {-# INLINE (*) #-}
instance Rg P.Double      where (*) = (P.*) ; {-# INLINE (*) #-}
instance Rg P.Rational    where (*) = (P.*) ; {-# INLINE (*) #-}

instance Rg b => Rg (a -> b) where
    {-# INLINE (*) #-}
    f*g = \a -> f a * g a

---------------------------------------

-- | A Rig is a Rg with multiplicative identity.
-- They are also known as semirings.
--
-- See <https://en.wikipedia.org/wiki/Semiring wikipedia>
-- and <http://ncatlab.org/nlab/show/rig ncatlab>
-- for more details.
class (Monoid r, Rg r) => Rig r where
    -- | the multiplicative identity
    one :: r

-- | FIXME: this should be in the Rig class, but putting it there requires a lot of changes to Eq
isOne :: (Rig g) => g -> P.Bool
isOne = (==one)

-- | FIXME: this should be in the Rig class, but putting it there requires a lot of changes to Eq
notOne :: (Rig g) => g -> P.Bool
notOne = (/=one)

law_Rig_multiplicativeId :: (Rig r) => r -> P.Bool
law_Rig_multiplicativeId r = r * one == r && one * r == r

instance Rig P.Int         where one = 1 ; {-# INLINE one #-}
instance Rig P.Integer     where one = 1 ; {-# INLINE one #-}
instance Rig P.Float       where one = 1 ; {-# INLINE one #-}
instance Rig P.Double      where one = 1 ; {-# INLINE one #-}
instance Rig P.Rational    where one = 1 ; {-# INLINE one #-}

instance Rig b => Rig (a -> b) where
    {-# INLINE one #-}
    one = \a -> one

---------------------------------------

-- | A "Ring" without identity.
type Rng r = (Rg r, Group r)

-- |
--
-- It is not part of the standard definition of rings that they have a "fromInteger" function.
-- It follows from the definition, however, that we can construct such a function.
-- The "slowFromInteger" function is this standard construction.
--
-- See <https://en.wikipedia.org/wiki/Ring_%28mathematics%29 wikipedia>
-- and <http://ncatlab.org/nlab/show/ring ncatlab>
-- for more details.
--
-- FIXME:
-- We can construct a "Module" from any ring by taking (*)=(.*.).
-- Thus, "Module" should be a superclass of "Ring".
-- Currently, however, this creates a class cycle, so we can't do it.
-- A number of type signatures are therefore more complicated than they need to be.
class (Rng r, Rig r) => Ring r where
    fromInteger :: P.Integer -> r
    fromInteger = slowFromInteger

defn_Ring_fromInteger :: (Ring r) => r -> P.Integer-> P.Bool
defn_Ring_fromInteger r i = fromInteger i `asTypeOf` r
                         == slowFromInteger i

-- | Here we construct an element of the Ring based on the additive and multiplicative identities.
-- This function takes O(n) time, where n is the size of the Integer.
-- Most types should be able to compute this value significantly faster.
--
-- FIXME: replace this with peasant multiplication.
slowFromInteger :: forall r. (Rng r, Rig r) => P.Integer -> r
slowFromInteger i = if i>0
    then          foldl' (+) zero $ P.map (const (one::r)) [1..        i]
    else negate $ foldl' (+) zero $ P.map (const (one::r)) [1.. negate i]

instance Ring P.Int         where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}
instance Ring P.Integer     where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}
instance Ring P.Float       where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}
instance Ring P.Double      where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}
instance Ring P.Rational    where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}

instance Ring b => Ring (a -> b) where
    {-# INLINE fromInteger #-}
    fromInteger i = \a -> fromInteger i

{-# INLINABLE indicator #-}
indicator :: Ring r => P.Bool -> r
indicator True = 1
indicator False = 0

-- | 'Integral' numbers can be formed from a wide class of things that behave
-- like integers, but intuitively look nothing like integers.
--
-- FIXME: All Fields are integral domains; should we make it a subclass?  This would have the (minor?) problem of making the Integral class have to be an approximate embedding.
-- FIXME: Not all integral domains are homomorphic to the integers (e.g. a field)
--
-- See wikipedia on <https://en.wikipedia.org/wiki/Integral_element integral elements>,
--  <https://en.wikipedia.org/wiki/Integral_domain integral domains>,
-- and the <https://en.wikipedia.org/wiki/Ring_of_integers ring of integers>.
class Ring a => Integral a where
    toInteger :: a -> P.Integer
    infixl 7  `quot`, `rem`

    -- | truncates towards zero
    {-# INLINE quot #-}
    quot :: a -> a -> a
    quot a1 a2 = fst (quotRem a1 a2)

    {-# INLINE rem #-}
    rem :: a -> a -> a
    rem a1 a2 = snd (quotRem a1 a2)

    quotRem :: a -> a -> (a,a)


    infixl 7 `div`, `mod`

    -- | truncates towards negative infinity
    {-# INLINE div #-}
    div :: a -> a -> a
    div a1 a2 = fst (divMod a1 a2)

    {-# INLINE mod #-}
    mod :: a -> a -> a
    mod a1 a2 = snd (divMod a1 a2)

    divMod :: a -> a -> (a,a)


law_Integral_divMod :: (Integral a) => a -> a -> P.Bool
law_Integral_divMod a1 a2 = if a2 /= 0
    then a2 * (a1 `div` a2) + (a1 `mod` a2) == a1
    else True

law_Integral_quotRem :: (Integral a) => a -> a -> P.Bool
law_Integral_quotRem a1 a2 = if a2 /= 0
    then a2 * (a1 `quot` a2) + (a1 `rem` a2) == a1
    else True

law_Integral_toFromInverse :: (Integral a) => a -> P.Bool
law_Integral_toFromInverse a = fromInteger (toInteger a) == a

{-# INLINE[1] fromIntegral #-}
fromIntegral :: (Integral a, Ring b) => a -> b
fromIntegral = fromInteger . toInteger

-- | FIXME:
-- This should be moved into the class hierarchy and generalized.
--
-- FIXME:
-- There are more efficient implementations available if you restrict m to powers of 2.
-- Is GHC smart enough to convert `rem` into bit shifts?
-- See for more possibilities:
-- http://stackoverflow.com/questions/3407012/c-rounding-up-to-the-nearest-multiple-of-a-number
{-# INLINE roundUpToNearest #-}
roundUpToNearest :: P.Int-> P.Int ->P.Int
roundUpToNearest m x = x + m - 1 - (x - 1) `rem` m
-- roundUpToNearest m x = if s==0
--     then
--     else x+r
--     where
--         s = x`rem`m
--         r = if s==0 then 0 else m-s

instance Integral P.Int where
    {-# INLINE div #-}
    {-# INLINE mod #-}
    {-# INLINE divMod #-}
    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE quotRem #-}
    {-# INLINE toInteger #-}
    div = P.div
    mod = P.mod
    divMod = P.divMod
    quot = P.quot
    rem = P.rem
    quotRem = P.quotRem
    toInteger = P.toInteger

instance Integral P.Integer where
    {-# INLINE div #-}
    {-# INLINE mod #-}
    {-# INLINE divMod #-}
    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE quotRem #-}
    {-# INLINE toInteger #-}
    div = P.div
    mod = P.mod
    divMod = P.divMod
    quot = P.quot
    rem = P.rem
    quotRem = P.quotRem
    toInteger = P.toInteger

instance Integral b => Integral (a -> b) where
    {-# INLINE div #-}
    {-# INLINE mod #-}
    {-# INLINE divMod #-}
    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE quotRem #-}
    {-# INLINE toInteger #-}
    quot f1 f2 = \a -> quot (f1 a) (f2 a)
    rem f1 f2 = \a -> rem (f1 a) (f2 a)
    quotRem f1 f2 = (quot f1 f2, rem f1 f2)

    div f1 f2 = \a -> div (f1 a) (f2 a)
    mod f1 f2 = \a -> mod (f1 a) (f2 a)
    divMod f1 f2 = (div f1 f2, mod f1 f2)

    -- FIXME
    toInteger = error "toInteger shouldn't be in the integral class b/c of bad function instance"

---------------------------------------

-- | Fields are Rings with a multiplicative inverse.
--
-- See <https://en.wikipedia.org/wiki/Field_%28mathematics%29 wikipedia>
-- and <http://ncatlab.org/nlab/show/field ncatlab>
-- for more details.
class Ring r => Field r where
    {-# INLINE reciprocal #-}
    reciprocal :: r -> r
    reciprocal r = one/r

    {-# INLINE (/) #-}
    infixl 7 /
    (/) :: r -> r -> r
    n/d = n * reciprocal d

--     infixr 5 /=
--     (/=) :: (PrimBase m) => Mutable m g -> g -> m ()
--     (/=) = immutable2mutable (/)

    {-# INLINE fromRational #-}
    fromRational :: P.Rational-> r
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

---------------------------------------

t1 :: P.Integer
t1 = 1 + 2

t2 :: P.Integer
t2 = zero + 2

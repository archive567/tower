> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE CPP #-}
> {-# LANGUAGE CPP,MagicHash,UnboxedTuples #-}
> {-# LANGUAGE PolyKinds #-}
>

This class list comes from [subhask]()

> module Tower (
>     -- * Number-like
>     -- ** Classes with one operator
>       Semigroup(..)
>     , Actor
>     , Action (..)
>     , (+.)
>     , Cancellative (..)
>     , Monoid (..)
>     , Abelian
>     , Group (..)
> 
>     -- ** Classes with two operators
>     , Rg(..)
>     , Rig(..)
>     , Rng
>     , Ring(..)
>     , slowFromInteger
>     , Integral(..)
>     , fromIntegral
>     , Field(..)
>     , OrdField
>     , RationalField(..)
>     , convertRationalField
>     , toFloat
>     , toDouble
>     , BoundedField(..)
>     , infinity
>     , negInfinity
>     , ExpRing (..)
>     , (^)
>     , ExpField (..)
>     , Real (..)
>     , QuotientField(..)
> 
>     -- ** Sizes
>     , Normed (..)
>     , abs
>     , Metric (..)
>     , isFartherThan
>     , lb2distanceUB
> 
>     -- ** Linear algebra
>     , Scalar
>     , IsScalar
>     , HasScalar
>     , type (><)
>     , Module (..)
>     , (*.)
>     , FreeModule (..)
>     , FiniteModule (..)
>     , VectorSpace (..)
>     , Banach (..)
>     , Hilbert (..)
>     , squaredInnerProductNorm
>     , innerProductDistance
>     , innerProductNorm
>     , TensorAlgebra (..)
> ) where


Using the [protolude]() prelude to keep track of dependencies.

> import Protolude ((.), asTypeOf, fst, snd, foldl, const, foldl', numerator, denominator, ($), error, undefined)
> import qualified Protolude as P

The main types that the tower hooks in to:

> import Protolude (
>     Int,
>     Integer,
>     Float,
>     Double,
>     Rational,
>     Eq(..),
>     Bool(..),
>     Ord(..),
>     Bounded(..))

plus
---

> class Semigroup g where
>     {-# MINIMAL (+) #-}
>     infixl 6 +
>     (+) :: g -> g -> g

When I first saw a monoid, my eyes glazed over the details in the midst of haskell concept overload. `mempty` was something that was empty I guessed, and m seemed to be a universal prefix shoved randomly throughout the haskell codebase.  And `mappend`, well that must be how you append lists I guess, maybe in a monad thing. And `<>` - I just saw anything wrapped in greater than less than signs as one ofthose things I'd have to look up later.

It took a year before I realised that mappend was just like adding up numbers, and mempty was just like 0.  In slightly shorter time, I discovered that <> was just like + - except you couldn't say that.

So, enough with the baby words and line noise, let's call them + and zero (and map!).

> instance Semigroup Int      where (+) = (P.+)
> instance Semigroup Integer  where (+) = (P.+)
> instance Semigroup Float    where (+) = (P.+)
> instance Semigroup Double   where (+) = (P.+)
> instance Semigroup Rational where (+) = (P.+)
> instance Semigroup b => Semigroup (a -> b) where
>     f+g = \a -> f a + g a

zero
===

tekmo thinks that we shouldn't separate + and zero.  In other words, that the positive integers without a zero is a poor tool.  The first 100,000 years of numerical analysis is on his side.

> class Semigroup g => Monoid g where
>     zero :: g
> 
> instance Monoid Int       where zero = 0
> instance Monoid Integer   where zero = 0
> instance Monoid Float     where zero = 0
> instance Monoid Double    where zero = 0
> instance Monoid Rational  where zero = 0
> 
> instance Monoid b => Monoid (a -> b) where
>     zero = \a -> zero
> 

There is a school of thought, of course, that the choice of plus and zero is an arbitrary one, because you can wire up times and one just as nice.  Well, not sure how other people did it, but I learnt my sums before my times.

minus
---

This allows us to define "subtraction" in the semigroup.
If the semigroup is embeddable in a group, subtraction can be thought of as performing the group subtraction and projecting the result back into the domain of the cancellative semigroup.
It is an open problem to fully characterize which cancellative semigroups can be embedded into groups.

See (http://en.wikipedia.org/wiki/Cancellative_semigroup) for more details.

> class Semigroup g => Cancellative g where
>     infixl 6 -
>     (-) :: g -> g -> g
> 
> instance Cancellative Int        where (-) = (P.-)
> instance Cancellative Integer    where (-) = (P.-)
> instance Cancellative Float      where (-) = (P.-)
> instance Cancellative Double     where (-) = (P.-)
> instance Cancellative Rational   where (-) = (P.-)
> instance Cancellative b => Cancellative (a -> b) where
>     f-g = \a -> f a - g a

group
---

> class (Cancellative g, Monoid g) => Group g where
>     negate :: g -> g
>     negate g = zero - g
> 
> instance Group Int
> instance Group Integer
> instance Group Float
> instance Group Double
> instance Group Rational
> instance Group b => Group (a -> b)

abelian
---

> class Semigroup m => Abelian m
> 
> instance Abelian Int
> instance Abelian Integer
> instance Abelian Float
> instance Abelian Double
> instance Abelian Rational
> instance Abelian b => Abelian (a -> b)
>

times
--

There is no standard terminology for this structure. They might also be called \"semirings without identity\", \"pre-semirings\", or \"hemirings\".

See (http://math.stackexchange.com/questions/359437/name-for-a-semiring-minus-multiplicative-identity-requirement) for a discussion on naming.

> class (Abelian r, Monoid r) => Rg r where
>     infixl 7 *
>     (*) :: r -> r -> r
> 
> instance Rg Int         where (*) = (P.*) 
> instance Rg Integer     where (*) = (P.*) 
> instance Rg Float       where (*) = (P.*) 
> instance Rg Double      where (*) = (P.*) 
> instance Rg Rational    where (*) = (P.*) 
> instance Rg b => Rg (a -> b) where
>     f*g = \a -> f a * g a
> 


one 
---

> -- | A Rig is a Rg with multiplicative identity.
> -- They are also known as semirings.
> --
> -- See <https://en.wikipedia.org/wiki/Semiring wikipedia>
> -- and <http://ncatlab.org/nlab/show/rig ncatlab>
> -- for more details.
> class (Monoid r, Rg r) => Rig r where
>     -- | the multiplicative identity
>     one :: r
> 
> instance Rig Int         where one = 1
> instance Rig Integer     where one = 1
> instance Rig Float       where one = 1
> instance Rig Double      where one = 1
> instance Rig Rational    where one = 1
> instance (Rig b) => Rig (a -> b) where
>     one = \a -> one
> 


rng
---

> 
> -- | A "Ring" without identity.
> type Rng r = (Rg r, Group r)
>

ring
---

It is not part of the standard definition of rings that they have a "fromInteger" function.
It follows from the definition, however, that we can construct such a function.
The "slowFromInteger" function is this standard construction.

See <https://en.wikipedia.org/wiki/Ring_%28mathematics%29 wikipedia>
and <http://ncatlab.org/nlab/show/ring ncatlab>
for more details.

FIXME:

> -- We can construct a "Module" from any ring by taking (*)=(.*.).
> -- Thus, "Module" should be a superclass of "Ring".
> -- Currently, however, this creates a class cycle, so we can't do it.
> -- A number of type signatures are therefore more complicated than they need to be.
> class (Rng r, Rig r) => Ring r where
>     fromInteger :: Integer -> r
>     fromInteger = slowFromInteger
> 
> -- | Here we construct an element of the Ring based on the additive and multiplicative identities.
> -- This function takes O(n) time, where n is the size of the Integer.
> -- Most types should be able to compute this value significantly faster.
> --
> -- FIXME: replace this with peasant multiplication.
> slowFromInteger :: forall r. (Rng r, Rig r) => Integer -> r
> slowFromInteger i = if i>0
>     then          foldl' (+) zero $ P.map (const (one::r)) [1..        i]
>     else negate $ foldl' (+) zero $ P.map (const (one::r)) [1.. negate i]
> 
> instance Ring Int         where fromInteger = fromInteger
> instance Ring Integer     where fromInteger = fromInteger
> instance Ring Float       where fromInteger = fromInteger
> instance Ring Double      where fromInteger = fromInteger
> instance Ring Rational    where fromInteger = fromInteger
> instance Ring b => Ring (a -> b) where
>     fromInteger i = \a -> fromInteger i


integral
---

> -- | 'Integral' numbers can be formed from a wide class of things that behave
> -- like integers, but intuitively look nothing like integers.
> --
> -- FIXME: All Fields are integral domains; should we make it a subclass?  This would have the (minor?) problem of making the Integral class have to be an approximate embedding.
> -- FIXME: Not all integral domains are homomorphic to the integers (e.g. a field)
> --
> -- See wikipedia on <https://en.wikipedia.org/wiki/Integral_element integral elements>,
> --  <https://en.wikipedia.org/wiki/Integral_domain integral domains>,
> -- and the <https://en.wikipedia.org/wiki/Ring_of_integers ring of integers>.


> class Ring a => Integral a where
>
>     toInteger :: a -> Integer
>     
>     infixl 7  `quot`, `rem`
> 
>     -- | truncates towards zero
>     quot :: a -> a -> a
>     quot a1 a2 = fst (quotRem a1 a2)
>     
>     rem :: a -> a -> a
>     rem a1 a2 = snd (quotRem a1 a2)
> 
>     quotRem :: a -> a -> (a,a)
>     
>     infixl 7 `div`, `mod`
> 
>     -- | truncates towards negative infinity
>     div :: a -> a -> a
>     div a1 a2 = fst (divMod a1 a2)
>     mod :: a -> a -> a
>     mod a1 a2 = snd (divMod a1 a2)
> 
>     divMod :: a -> a -> (a,a)
> 
> 
> instance Integral Int where
>     div = P.div
>     mod = P.mod
>     divMod = P.divMod
>     quot = P.quot
>     rem = P.rem
>     quotRem = P.quotRem
>     toInteger = P.toInteger
> 
> instance Integral Integer where
>     div = P.div
>     mod = P.mod
>     divMod = P.divMod
>     quot = P.quot
>     rem = P.rem
>     quotRem = P.quotRem
>     toInteger = P.toInteger
> 
> instance Integral b => Integral (a -> b) where
>     quot f1 f2 = \a -> quot (f1 a) (f2 a)
>     rem f1 f2 = \a -> rem (f1 a) (f2 a)
>     quotRem f1 f2 = (quot f1 f2, rem f1 f2)
>     div f1 f2 = \a -> div (f1 a) (f2 a)
>     mod f1 f2 = \a -> mod (f1 a) (f2 a)
>     divMod f1 f2 = (div f1 f2, mod f1 f2)

> fromIntegral :: (Integral a, Ring b) => a -> b
> fromIntegral = fromInteger . toInteger

field
---

> -- | Fields are Rings with a multiplicative inverse.
> --
> -- See <https://en.wikipedia.org/wiki/Field_%28mathematics%29 wikipedia>
> -- and <http://ncatlab.org/nlab/show/field ncatlab>
> -- for more details.
> class Ring r => Field r where
>     reciprocal :: r -> r
>     reciprocal r = one/r
>     
>     infixl 7 /
>     (/) :: r -> r -> r
>     n/d = n * reciprocal d
>     
>     fromRational :: Rational -> r
>     fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
>
> 
> instance Field Float where 
>     (/) = (P./)
>     fromRational=P.fromRational
>     
> instance Field Double where 
>     (/) = (P./)
>     fromRational=P.fromRational
>     
> instance Field Rational where 
>     (/) = (P./)
>     fromRational=P.fromRational
> 
> instance Field b => Field (a -> b) where
>     reciprocal f = reciprocal . f

ordered fields
---

> -- | Ordered fields are generalizations of the rational numbers that maintain most of the nice properties.
> -- In particular, all finite fields and the complex numbers are NOT ordered fields.
> --
> -- See <http://en.wikipedia.org/wiki/Ordered_field wikipedia> for more details.
> class (Field r, Ord r, Normed r, IsScalar r) => OrdField r
> 
> instance OrdField Float
> instance OrdField Double
> instance OrdField Rational
> 

bounded field
---

> -- | The prototypical example of a bounded field is the extended real numbers.
> -- Other examples are the extended hyperreal numbers and the extended rationals.
> -- Each of these fields has been extensively studied, but I don't know of any studies of this particular abstraction of these fields.
> --
> -- See <https://en.wikipedia.org/wiki/Extended_real_number_line wikipedia> for more details.


> class (OrdField r, Bounded r) => BoundedField r where
>     nan :: r
>     nan = zero/zero
> 
>     isNaN :: r -> Bool
> 
> infinity :: BoundedField r => r
> infinity = maxBound
> 
> negInfinity :: BoundedField r => r
> negInfinity = minBound
> 
> 
> instance Bounded Float  where
>     maxBound = 1/0
>     minBound = -1/0
> 
> instance Bounded Double where
>     maxBound = 1/0
>     minBound = -1/0
> 
> instance BoundedField Float  where isNaN = P.isNaN
> instance BoundedField Double where isNaN = P.isNaN
>

rational field
---

> -- | A Rational field is a field with only a single dimension.
> --
> -- FIXME: this isn't part of standard math; why is it here?

> class Field r => RationalField r where
>     toRational :: r -> Rational
> 
> instance RationalField Float    where  toRational=P.toRational
> instance RationalField Double   where  toRational=P.toRational
> instance RationalField Rational where  toRational=P.toRational
> 
> {-# INLINE convertRationalField #-}
> convertRationalField :: (RationalField a, RationalField b) => a -> b
> convertRationalField = fromRational . toRational
> 
> -- |
> --
> -- FIXME:
> -- These functions don't work for Int's, but they should
> toFloat :: RationalField a => a -> Float
> toFloat = convertRationalField
> 
> toDouble :: RationalField a => a -> Double
> toDouble = convertRationalField
> 


quotient field
---

> -- | A 'QuotientField' is a field with an 'IntegralDomain' as a subring.
> -- There may be many such subrings (for example, every field has itself as an integral domain subring).
> -- This is especially true in Haskell because we have different data types that represent essentially the same ring (e.g. "Int" and "Integer").
> -- Therefore this is a multiparameter type class.
> -- The 'r' parameter represents the quotient field, and the 's' parameter represents the subring.
> -- The main purpose of this class is to provide functions that map elements in 'r' to elements in 's' in various ways.
> --
> -- FIXME: Need examples.  Is there a better representation?
> --
> -- See <http://en.wikipedia.org/wiki/Field_of_fractions wikipedia> for more details.
> --
> class (Ring r, Integral s) => QuotientField r s where
>     truncate    :: r -> s
>     round       :: r -> s
>     ceiling     :: r -> s
>     floor       :: r -> s
> 
>     (^^)        :: r -> s -> r
> 
> #define mkQuotientField(r,s) \
> instance QuotientField r s where \
>     truncate = P.truncate; \
>     round    = P.round; \
>     ceiling  = P.ceiling; \
>     floor    = P.floor; \
>     (^^)     = (P.^^); \
> 
> mkQuotientField(Float,Int)
> mkQuotientField(Float,Integer)
> mkQuotientField(Double,Int)
> mkQuotientField(Double,Integer)
> mkQuotientField(Rational,Int)
> mkQuotientField(Rational,Integer)
> 
> 
> instance QuotientField b1 b2 => QuotientField (a -> b1) (a -> b2) where
>     truncate f = \a -> truncate $ f a
>     round f = \a -> round $ f a
>     ceiling f = \a -> ceiling $ f a
>     floor f = \a -> floor $ f a
>     (^^) f1 f2 = \a -> (^^) (f1 a) (f2 a)
> 

exponents
---

> -- | Rings augmented with the ability to take exponents.
> --
> -- Not all rings have this ability.
> -- Consider the ring of rational numbers (represented by "Rational" in Haskell).
> -- Raising any rational to an integral power results in another rational.
> -- But raising to a fractional power results in an irrational number.
> -- For example, the square root of 2.
> --
> -- See <http://en.wikipedia.org/wiki/Exponential_field#Exponential_rings wikipedia> for more detail.
> --
> -- FIXME:
> -- This class hierarchy doesn't give a nice way exponentiate the integers.
> -- We need to add instances for all the quotient groups.
> class Ring r => ExpRing r where
>     (**) :: r -> r -> r
>     infixl 8 **
> 
>     logBase :: r -> r -> r
> 
> -- | An alternate form of "(**)" that some people find more convenient.
> (^) :: ExpRing r => r -> r -> r
> (^) = (**)
> 
> instance ExpRing Float where
>     (**) = (P.**)
>     logBase = P.logBase
> 
> instance ExpRing Double where
>     (**) = (P.**)
> 
>     logBase = P.logBase
> 

logs
---

> 
> -- | Fields augmented with exponents and logarithms.
> --
> -- Technically, there are fields for which only a subset of the functions below are meaningful.
> -- But these fields don't have any practical computational uses that I'm aware of.
> -- So I've combined them all into a single class for simplicity.
> --
> -- See <http://en.wikipedia.org/wiki/Exponential_field wikipedia> for more detail.
> class (ExpRing r, Field r) => ExpField r where
>     sqrt :: r -> r
>     sqrt r = r**(one/one+one)
> 
>     exp :: r -> r
>     log :: r -> r
> 
> instance ExpField Float where
>     sqrt = P.sqrt
>     log = P.log
>     exp = P.exp
> 
> instance ExpField Double where
>     sqrt = P.sqrt
>     log = P.log
>     exp = P.exp
> 

real
---

> -- | This is a catch-all class for things the real numbers can do but don't exist in other classes.
> --
> -- FIXME:
> -- Factor this out into a more appropriate class hierarchy.
> -- For example, some (all?) trig functions need to move to a separate class in order to support trig in finite fields (see <https://en.wikipedia.org/wiki/Trigonometry_in_Galois_fields wikipedia>).
> --
> -- FIXME:
> -- This class is misleading/incorrect for complex numbers.
> --
> -- FIXME:
> -- There's a lot more functions that need adding.


> class ExpField r => Real r where
>     pi :: r
>     sin :: r -> r
>     cos :: r -> r
>     tan :: r -> r
>     asin :: r -> r
>     acos :: r -> r
>     atan :: r -> r
>     sinh :: r -> r
>     cosh :: r -> r
>     tanh :: r -> r
>     asinh :: r -> r
>     acosh :: r -> r
>     atanh :: r -> r
> 
> instance Real Float where
> 
>     pi = P.pi
>     sin = P.sin
>     cos = P.cos
>     tan = P.tan
>     asin = P.asin
>     acos = P.acos
>     atan = P.atan
>     sinh = P.sinh
>     cosh = P.cosh
>     tanh = P.tanh
>     asinh = P.asinh
>     acosh = P.acosh
>     atanh = P.atanh
> 
> instance Real Double where
>     pi = P.pi
>     sin = P.sin
>     cos = P.cos
>     tan = P.tan
>     asin = P.asin
>     acos = P.acos
>     atan = P.atan
>     sinh = P.sinh
>     cosh = P.cosh
>     tanh = P.tanh
>     asinh = P.asinh
>     acosh = P.acosh
>     atanh = P.atanh
> 

scalar
---

> type family Scalar m
> 
> infixr 8 ><
> type family (><) (a::k1) (b::k2) :: *
> type instance Int       >< Int        = Int
> type instance Integer   >< Integer    = Integer
> type instance Float     >< Float      = Float
> type instance Double    >< Double     = Double
> type instance Rational  >< Rational   = Rational
> type instance (a -> b)  >< c          = a -> (b><c)
> 
> -- | A synonym that covers everything we intuitively thing scalar variables should have.
> type IsScalar r = (Ring r, Ord r, Scalar r~r, Normed r, r~(r><r))
> 
> -- | A (sometimes) more convenient version of "IsScalar".
> type HasScalar a = IsScalar (Scalar a)
> 
> type instance Scalar Int      = Int
> type instance Scalar Integer  = Integer
> type instance Scalar Float    = Float
> type instance Scalar Double   = Double
> type instance Scalar Rational = Rational
> 
> type instance Scalar (a,b) = Scalar a
> type instance Scalar (a,b,c) = Scalar a
> type instance Scalar (a,b,c,d) = Scalar a
> type instance Scalar (a -> b) = Scalar b
> 

normed
---

> 
> -- | FIXME: What constraint should be here? Semigroup?
> --
> -- See <http://ncatlab.org/nlab/show/normed%20group ncatlab>
> class
>     ( Ord (Scalar g)
>     , Scalar (Scalar g) ~ Scalar g
>     , Ring (Scalar g)
>     ) => Normed g where
>     size :: g -> Scalar g
> 
>     sizeSquared :: g -> Scalar g
>     sizeSquared g = s*s
>         where
>             s = size g
> 
> abs :: IsScalar g => g -> g
> abs = size
> 
> instance Normed Int       where size = P.abs
> instance Normed Integer   where size = P.abs
> instance Normed Float     where size = P.abs
> instance Normed Double    where size = P.abs
> instance Normed Rational  where size = P.abs
> 


module
---

> 
> class
>     ( Abelian v
>     , Group v
>     , HasScalar v
>     , v ~ (v><Scalar v)
>     ) => Module v
>         where
> 
>     -- | Scalar multiplication.
>     infixl 7 .*
>     (.*) :: v -> Scalar v -> v
> 
> {-# INLINE (*.) #-}
> infixl 7 *.
> (*.) :: Module v => Scalar v -> v -> v
> r *. v  = v .* r
> 
> instance Module Int       where (.*) = (*)
> instance Module Integer   where (.*) = (*)
> instance Module Float     where (.*) = (*)
> instance Module Double    where (.*) = (*)
> instance Module Rational  where (.*) = (*)
> 
> instance
>     ( Module b
>     ) => Module (a -> b)
>         where
>     f .*  b = \a -> f a .*  b
>
>
>

free module
---

> -- | Free modules have a basis.
> -- This means it makes sense to perform operations elementwise on the basis coefficients.
> --
> -- See <https://en.wikipedia.org/wiki/Free_module wikipedia> for more detail.
> class Module v => FreeModule v where
> 
>     -- | Multiplication of the components pointwise.
>     -- For matrices, this is commonly called Hadamard multiplication.
>     --
>     -- See <http://en.wikipedia.org/wiki/Hadamard_product_%28matrices%29 wikipedia> for more detail.
>     --
>     -- FIXME: This is only valid for modules with a basis.
>     infixl 7 .*.
>     (.*.) :: v -> v -> v
> 
>     -- | The identity for Hadamard multiplication.
>     -- Intuitively, this object has the value "one" in every column.
>     ones :: v
> 
> instance FreeModule Int       where (.*.) = (*); ones = one
> instance FreeModule Integer   where (.*.) = (*); ones = one
> instance FreeModule Float     where (.*.) = (*); ones = one
> instance FreeModule Double    where (.*.) = (*); ones = one
> instance FreeModule Rational  where (.*.) = (*); ones = one
> 
> instance
>     ( FreeModule b
>     ) => FreeModule (a -> b)
>         where
>     g .*. f = \a -> g a .*. f a
>     ones = \_ -> ones
> 
> ---------------------------------------
>
>

finite module
---

> -- | If our "FreeModule" has a finite basis, then we can:
> --
> -- * index into the modules basis coefficients
> --
> -- * provide a dense construction method that's a bit more convenient than "fromIxList".


> class
>     ( FreeModule v
>     ) => FiniteModule v
>         where
>     -- | Returns the dimension of the object.
>     -- For some objects, this may be known statically, and so the parameter will not be "seq"ed.
>     -- But for others, this may not be known statically, and so the parameter will be "seq"ed.
>     dim :: v -> Int
> 
>     unsafeToModule :: [Scalar v] -> v
> 
> instance FiniteModule Int       where dim _ = 1; unsafeToModule [x] = x
> instance FiniteModule Integer   where dim _ = 1; unsafeToModule [x] = x
> instance FiniteModule Float     where dim _ = 1; unsafeToModule [x] = x
> instance FiniteModule Double    where dim _ = 1; unsafeToModule [x] = x
> instance FiniteModule Rational  where dim _ = 1; unsafeToModule [x] = x
> 
> ---------------------------------------

vector space
---

> 
> class (FreeModule v, Field (Scalar v)) => VectorSpace v where
> 
>     infixl 7 ./
>     (./) :: v -> Scalar v -> v
>     v ./ r = v .* reciprocal r
> 
>     infixl 7 ./.
>     (./.) :: v -> v -> v
> 
> instance VectorSpace Float     where (./) = (/); (./.) = (/)
> instance VectorSpace Double    where (./) = (/); (./.) = (/)
> instance VectorSpace Rational  where (./) = (/); (./.) = (/)
> 
> instance VectorSpace b => VectorSpace (a -> b) where g ./. f = \a -> g a ./. f a
> 

banach
---

> -- | A Banach space is a Vector Space equipped with a compatible Norm and Metric.
> --
> -- See <http://en.wikipedia.org/wiki/Banach_space wikipedia> for more details.
> class (VectorSpace v, Normed v, Metric v) => Banach v where
>     normalize :: v -> v
>     normalize v = v ./ size v
> 
> instance Metric Float    where distance x1 x2 = abs $ x1 - x2
> instance Metric Double   where distance x1 x2 = abs $ x1 - x2
> instance Metric Rational where distance x1 x2 = abs $ x1 - x2
> 
> instance Banach Float
> instance Banach Double
> instance Banach Rational
> 

hilbert
---

> -- | Hilbert spaces are a natural generalization of Euclidean space that allows for infinite dimension.
> --
> -- See <http://en.wikipedia.org/wiki/Hilbert_space wikipedia> for more details.
> --
> -- FIXME:
> -- The result of a dot product must always be an ordered field.
> -- This is true even when the Hilbert space is over a non-ordered field like the complex numbers.
> -- But the "OrdField" constraint currently prevents us from doing scalar multiplication on Complex Hilbert spaces.
> -- See <http://math.stackexchange.com/questions/49348/inner-product-spaces-over-finite-fields> and <http://math.stackexchange.com/questions/47916/banach-spaces-over-fields-other-than-mathbbc> for some technical details.
> class ( Banach v , TensorAlgebra v , Real (Scalar v), OrdField (Scalar v) ) => Hilbert v where
>     infix 8 <>
>     (<>) :: v -> v -> Scalar v
> 
> instance Hilbert Float    where (<>) = (*)
> instance Hilbert Double   where (<>) = (*)
> 
> {-# INLINE squaredInnerProductNorm #-}
> squaredInnerProductNorm :: Hilbert v => v -> Scalar v
> squaredInnerProductNorm v = v<>v
> 
> {-# INLINE innerProductNorm #-}
> innerProductNorm :: Hilbert v => v -> Scalar v
> innerProductNorm = undefined -- sqrt . squaredInnerProductNorm
> 
> {-# INLINE innerProductDistance #-}
> innerProductDistance :: Hilbert v => v -> v -> Scalar v
> innerProductDistance _ _ = undefined --innerProductNorm $ v1-v2
> 

tensor algebra
---

:tada:

> -- | Tensor algebras generalize the outer product of vectors to construct a matrix.
> --
> -- See <https://en.wikipedia.org/wiki/Tensor_algebra wikipedia> for details.
> --
> -- FIXME:
> -- This needs to be replaced by the Tensor product in the Monoidal category Vect
> class
>     ( VectorSpace v
>     , VectorSpace (v><v)
>     , Scalar (v><v) ~ Scalar v
>     , Normed (v><v)     -- the size represents the determinant
>     , Field (v><v)
>     ) => TensorAlgebra v
>         where
> 
>     -- | Take the tensor product of two vectors
>     (><) :: v -> v -> (v><v)
> 
>     -- | "left multiplication" of a square matrix
>     vXm :: v -> (v><v) -> v
> 
>     -- | "right multiplication" of a square matrix
>     mXv :: (v><v) -> v -> v
> 
> instance TensorAlgebra Float    where  (><) = (*); vXm = (*);  mXv = (*)
> instance TensorAlgebra Double   where  (><) = (*); vXm = (*);  mXv = (*)
> instance TensorAlgebra Rational where  (><) = (*); vXm = (*);  mXv = (*)
> 

metric
---

> -- | Metric spaces give us the most intuitive notion of distance between objects.
> --
> -- FIXME: There are many other notions of distance and we should make a whole hierarchy.
> class
>     ( HasScalar v
>     , Eq v
>     , (Scalar v) ~ v
>     ) => Metric v
>         where
> 
>     distance :: v -> v -> Scalar v
> 
>     -- | If the distance between two datapoints is less than or equal to the upper bound,
>     -- then this function will return the distance.
>     -- Otherwise, it will return some number greater than the upper bound.
>     distanceUB :: v -> v -> Scalar v -> Scalar v
>     distanceUB v1 v2 _ = distance v1 v2
> 
> -- | Calling this function will be faster on some 'Metric's than manually checking if distance is greater than the bound.
> isFartherThan :: Metric v => v -> v -> Scalar v -> Bool
> isFartherThan s1 s2 b = distanceUB s1 s2 b > b
> 
> -- | This function constructs an efficient default implementation for 'distanceUB' given a function that lower bounds the distance metric.
> lb2distanceUB ::
>     ( Metric a
>     ) => (a -> a -> Scalar a)
>       -> (a -> a -> Scalar a -> Scalar a)
> lb2distanceUB lb p q b = if lbpq > b
>     then lbpq
>     else distance p q
>     where
>         lbpq = lb p q

semigroup [actions](https://en.wikipedia.org/wiki/Semigroup_action)
---

A semigroup that acts on a type.

> type family Actor s
> 
> type instance Actor Int      = Int
> type instance Actor Integer  = Integer
> type instance Actor Float    = Float
> type instance Actor Double   = Double
> type instance Actor Rational = Rational
> type instance Actor (a->b)   = a->Actor b

Semigroup actions let us apply a semigroup to a set. The theory of Modules is essentially the theory of Ring actions. See [mathoverflow](http://mathoverflow.net/questions/100565/why-are-ring-actions-much-harder-to-find-than-group-actions)

> -- FIXME: We would like every Semigroup to act on itself, but this results in a class cycle.
> class (Semigroup (Actor s)) => Action s where
>     infixl 6 .+
>     (.+) :: s -> Actor s -> s
> 
> infixr 6 +.
> (+.) :: Action s => Actor s -> s -> s
> a +. s = s .+ a
> 
> instance Action Int      where (.+) = (+)
> instance Action Integer  where (.+) = (+)
> instance Action Float    where (.+) = (+)
> instance Action Double   where (.+) = (+)
> instance Action Rational where (.+) = (+)
> instance Action b => Action (a->b) where
>     f.+g = \x -> f x.+g x

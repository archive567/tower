<meta charset="utf-8">
<link rel="stylesheet" href="other/lhs.css">
<script type="text/javascript" async
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>

[tower](https://github.com/tonyday567/tower)
===

![](other/tower.png)

This is a numeric tower built with:

- boiler-plate category theory
- the subhask numeric tower, stripped of mutability and sub-category concepts.
- Semigroups operator as '+'
- Monoid operator as zero
- Protolude as a reference point for fitting in with the rest of the haskell ecosystem.

> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE CPP #-}
> {-# LANGUAGE PolyKinds #-}

Much of this class list is the same as [subhask](https://github.com/mikeizbicki/subhask) as is much of the commentary.

> module Tower where

> import qualified Protolude as P
> import Protolude (
>     Int,
>     Integer,
>     Float,
>     Double,
>     Rational,
>     Eq(..),
>     Bool(..),
>     Ord(..),
>     Bounded(..),
>     (.), ($), undefined, Maybe(..))
>

> -- | Grown out of the flames, the magma 
> class Magma a where mul :: a -> a -> a
> class Magma a => Idempotent a
> class Magma a => Commutative a
> class Magma a => Associative a
> class Magma a => Unital a where unit :: a
> class Magma a => Invertible a where inv :: a -> a
> class (Magma a, Magma b) => Homomorphic a b where hom :: a -> b
> instance Magma a => Homomorphic a a where hom a = a
> type Semigroup a = Associative a
> type Monoid a = (Unital a, Semigroup a)
> type Group a = (Invertible a, Monoid a)
> type Abelian c a = (Commutative a, c a)

> -- | Grown out of the flames, the magma 
> class AdditiveMagma a where plus :: a -> a -> a
> class AdditiveMagma a => AdditiveIdempotent a
> class AdditiveMagma a => AdditiveCommutative a
> class AdditiveMagma a => AdditiveAssociative a
> class AdditiveMagma a => AdditiveUnital a where plusunit :: a
> class AdditiveMagma a => AdditiveInvertible a where plusinv :: a -> a
> class (AdditiveMagma a, AdditiveMagma b) => AdditiveHomomorphic a b where
>     plushom :: a -> b
> instance AdditiveMagma a => AdditiveHomomorphic a a where plushom a = a
> type AdditiveSemigroup a = AdditiveAssociative a
> type AdditiveMonoid a = (AdditiveUnital a, AdditiveSemigroup a)
> type AdditiveGroup a = (AdditiveInvertible a, AdditiveMonoid a)
> type AdditiveAbelian c a = (AdditiveCommutative a, c a)

> -- | Grown out of the flames, the magma 
> class MultiplicativeMagma a where times :: a -> a -> a
> class MultiplicativeMagma a => MultiplicativeIdempotent a
> class MultiplicativeMagma a => MultiplicativeCommutative a
> class MultiplicativeMagma a => MultiplicativeAssociative a
> class MultiplicativeMagma a => MultiplicativeUnital a where timesunit :: a
> class MultiplicativeMagma a => MultiplicativeInvertible a where timesinv :: a -> a
> class (MultiplicativeMagma a, MultiplicativeMagma b) =>
>     MultiplicativeHomomorphic a b where
>     timeshom :: a -> b
> instance MultiplicativeMagma a => MultiplicativeHomomorphic a a where
>     timeshom a = a
> type MultiplicativeSemigroup a = MultiplicativeAssociative a
> type MultiplicativeMonoid a = (MultiplicativeUnital a, MultiplicativeSemigroup a)
> type MultiplicativeGroup a = (MultiplicativeInvertible a, MultiplicativeMonoid a)
> type MultiplicativeAbelian c a = (MultiplicativeCommutative a, c a)

> class (AdditiveCommutative a, AdditiveMonoid a) => Additive a where
>     zero :: a
>     zero = plusunit

> infixr 6 +
> (+) :: Additive a => a -> a -> a
> a + b = plus a b

> infixr 6 -
> (-) :: (Additive a, AdditiveInvertible a) => a -> a -> a
> (-) a b = a + plusinv b

> class (MultiplicativeCommutative a, MultiplicativeMonoid a) => Multiplicative a where
>     one :: a
>     one = timesunit

> infixr 7 *
> (*) :: Multiplicative a => a -> a -> a
> a * b = times a b

> infixr 7 /
> (/) :: (Multiplicative a, MultiplicativeInvertible a) => a -> a -> a
> (/) a b = a * timesinv b

> class (
>     Additive a
>   , Multiplicative a
>   ) => Distributive a

> class (Multiplicative r, Group r) => Ring r
>

> class (
>     Additive r
>   , Additive m
>   , AdditiveHomomorphic r m
>   ) => AdditiveModule r m
> 
> infixr 7 .+
> (.+) :: AdditiveModule r m => r -> m -> m
> r .+ m = plushom r + m 

> class (
>     Multiplicative r
>   , Multiplicative m
>   , MultiplicativeHomomorphic r m
>   ) => MultiplicativeModule r m
> 
> infixr 7 .*
> (.*) :: MultiplicativeModule r m => r -> m -> m
> r .* m = timeshom r * m 
>

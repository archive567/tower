[tower](https://tonyday567.github.com/tower) [![Build Status](https://travis-ci.org/tonyday567/tower.png)](https://travis-ci.org/tonyday567/tower)
===


[Magma](https://en.wikipedia.org/wiki/Magma_(algebra))
---

A magma is a tuple (t,⊕) consisting of a type t, and a function (⊕) :: t -> t -> t

The mathematical laws that apply for a magma are that:

- ⊕ is defined for all possible pairs of type t (T,T), and
- ⊕ is closed in the set of all possible values of type t (T). ie

    ∀ a, b ∈ T: a ⊕ b ∈ T

These laws are true by construction in haskell: the type signature and the above mathematical formulation are synonyms.

An example of a magma with no other laws is a vector cross-product in R3:

    cross :: (a,a,a) -> (a,a,a) -> (a,a,a)
    cross (a,b,c) (x,y,z) = (bz−cy,cx−az,bx−ay)

`cross` is closed, but is not unital, associative, commutative nor idempotent. (It is Unipotent, but that's another story)

Free Magma
---

data Tree a = Leaf a | Branch (Tree a) (Tree a)

https://www.schoolofhaskell.com/user/bss/magma-tree



looking for homes
---

https://bartoszmilewski.com/2014/09/29/how-to-get-enriched-over-magmas-and-monoids/

https://xinitrc.de/blog/2014/02/09/Sucker-for-generality.html

Common magmas (a -> a -> a):

- max
- min
- asTypeOf
- mappend
- +
- -
- *
- /
- quot
- rem
- div
- mod
- \
- **
- logBase
- atan2
- gcd
- lcm
- mplus
- union
- intersection
- difference
- &&
- ||
- append (bytestring)
- strip
- prepend
- chunk (text)
- ++


mathematical binary operations that are not magmas (a -> a -> b)

- ==
- <
- <=
- >
- >=
- /=
- - (for some types)
- / (for some types)

mathematical binary operations that are not magmas (a -> b -> a)

- :
- ^
- ^^
- const
- seq

homotype unary mathematical operations (a -> a)

- not
- succ
- pred
- abs
- signum
- id
- exp
- log
- sqrt
- sin
- cos
- tan
- recip
- negate

unary mathematical operations (a -> b)

- even
- odd
- ceiling
- truncate
- round
- floor
- fromIntegral
- toInteger
- fromEnum

unicode/formulae examples
---

https://www.stackage.org/haddock/lts-7.16/base-unicode-symbols-0.2.2.4/Prelude-Unicode.html
https://www.stackage.org/haddock/lts-7.16/math-functions-0.2.1.0/Numeric-Series.html


[Idempotent](https://en.wikipedia.org/wiki/Idempotence)
---

[Homomorphic](https://en.wikipedia.org/wiki/Homomorphism)
---

[Tensor Algebra](https://en.wikipedia.org/wiki/Tensor_algebra)
---


usage
---

rendered with:

~~~
stack build && pandoc -f markdown+lhs -i src/Tower.lhs -t html -o index.html --filter pandoc-include --mathjax
~~~






> {-# LANGUAGE NoImplicitPrelude #-}
> import Protolude
> import Tower.Prelude

> main :: IO ()
> main = print "a numeric tower"
>

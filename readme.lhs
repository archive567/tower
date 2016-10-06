tower
===

[![Build Status](https://travis-ci.org/tonyday567/tower.png)](https://travis-ci.org/tonyday567/tower)

tower
---

Numerical tower with:

- boiler-plate category theory tower
- subhask numeric tower, stripped of mutability and sub-category concepts.
- starting at Semigroups as '+'
- Protolude as reference point for how it all fits in with the rest of the haskell ecosystem.

> import Protolude hiding ((+),(-),(*),(/),zero,one,negate)
> import Tower
> 
>
> main :: IO ()
> main = do
>   print $ (zero + zero :: Integer)
>   print $ (zero + 2 :: Integer)
>   -- print $ (one * zero :: Double)
>   -- print $ (negate 2.3 :: Float)
>   -- print $ (zero/zero :: Integer)

usage
---

~~~
stack install && readme && pandoc -f markdown+lhs -i readme.lhs -t markdown -o readme.md --filter pandoc-include --mathjax
~~~

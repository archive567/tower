tower
===

plus
---

When I first saw a monoid, my eyes glazed over the details in the midst of haskell concept overload. `mempty` was something that was empty I guessed, and m seemed to be a universal prefix shoved randomly throughout the haskell codebase.  And `mappend`, well that must be how you append lists I guess, maybe in a monad thing. And `<>` - I just saw anything wrapped in greater than less than signs as one ofthose things I'd have to look up later.

It took a year before I realised that mappend was just like adding up numbers, and mempty was just like 0.  In slightly shorter time, I discovered that <> was just like + - except you couldn't say that.

So, enough with the baby words and line noise, let's call them + and zero (and map!).

This is a numeric tower built with:

- boiler-plate category theory
- the subhask numeric tower, stripped of mutability and sub-category concepts.
- Semigroups operator as '+'
- Monoid operator as zero
- Protolude as a reference point for fitting in with the rest of the haskell ecosystem.


> import Protolude hiding ((+),(-),(*),(/),zero,one,negate)
> import Tower
> import Tower.Ring
> import Tower.Vector
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

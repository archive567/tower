tower
===

[![Build Status](https://travis-ci.org/tonyday567/tower.png)](https://travis-ci.org/tonyday567/tower)

tower
---

usage
---

To build & run:

~~~
stack install && readme
~~~

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

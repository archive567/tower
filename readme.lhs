[tower](https://tonyday567.github.com/tower) [![Build Status](https://travis-ci.org/tonyday567/tower.png)](https://travis-ci.org/tonyday567/tower)
===

> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> import Tower.Prelude
>

The tower prelude uses the monoid and semigroup from protolude, rather than the tower versions.

>
> main :: IO ()
> main = do

To use literal numbers with tower without a type annotation somewhere, type defaulting as per Num needs to be applied to Additive.

    :t 1 + 1
    1 + 1 :: (Additive a, Num a) => a
  
>   putStrLn $ "1 + 1      = " ++ show (1 + 1 :: Int)
>   putStrLn $ "1.0 + 1.0  = " ++ show (1.0 + 1.0 :: Float)

`-` as a unary function, and a minus literal eg `-1` is treated specially by ghc, and also differently to one another.

    :t - 1
    - 1 :: Num a => a
    :t -1
    -1 :: Num t => t

The upshot of this, is that the unary function `-` is not yet hooking into tower, despite `(-)` and `negate` being hidden in the import of protolude.

>   putStrLn $ "-1         = " ++ show (- 1)

The binary function `-` is ok.

>   putStrLn $ "1 - 1      = " ++ show (1 - 1 :: Int)
>   putStrLn $ "1.0 - 1.0  = " ++ show (1.0 - 1.0 :: Float)
>   putStrLn $ "1 * 1      = " ++ show (1 * 1 :: Int)
>   putStrLn $ "1.0 * 1.0  = " ++ show (1.0 * 1.0 :: Float)
>   putStrLn $ ("1 / 1 is a type error" :: Text)
>   putStrLn $ "1.0 / 1.0  = " ++ show (1.0 / 1.0 :: Float)

Using `zero` and `one` rather than magical `0`s and `1`s in code is great practice.

>   putStrLn $ "zero       = " ++ show (zero :: Float)
>   putStrLn $ "one        = " ++ show (one :: Float)

BoundedField ensures that divide by zero works.  Having said this, actually printing one/zero as `Infinity` and -one/zero as `-Infinity` ain't coming from tower.
 
>   putStrLn $ "one / zero = " ++ show (one / zero :: Float)
>   putStrLn $ "(zero / zero) + one = " ++ show (zero / zero + one :: Float)



{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- ExtendedDefaultRules lets us us the usual operators without having to specify the types of literal numbers.
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tower.Examples where

-- `Tower.Prelude` is a drop-in replacement for `Prelude`.  Behind the scenes, it wraps `Protolude`.
import Tower.Prelude

-- * Integrals
--
-- |
-- >>> zero == 0
-- True
--
-- prop \a -> a + 0 == a
-- True
-- prop \a -> 0 + a == a
-- True
--
-- | Additive
--
-- >>> 1 + 1
-- 2
--
-- |
-- >>> 1 - 1
-- 0
--
-- |
-- >>> 1 * 1
-- 1
--
-- |
-- >>> 1 / 1
-- 1.0
--
-- Integrals should error on application of (/)
-- |
-- >>> 1 / (1::Int)
-- ...
-- ... No instance for (MultiplicativeGroup Int)
-- ... arising from a use of ‘/’
-- ...
-- 
-- |
-- >>> 1 `div` 2
-- 0
--
-- |
-- >>> 1 `mod` 2
-- 1
--
-- * Rings
--
-- |
-- >>> zero == 0.0
-- True
--
-- prop \a -> a + 0.0 == a
-- True
-- prop \a -> 0.0 + a == a
-- True
--
-- | 
--
-- >>> 1.0 + 1.0
-- 2.0
--
-- |
-- >>> 1.0 - 1.0
-- 0.0
--
-- |
-- checking unary minus is ok
-- >>> :t - 1
-- - 1 :: Num a => a
-- 
-- |
-- >>> 1.0 * 1.0
-- 1.0
--
-- |
-- >>> 1.0 / 1.0
-- 1.0
--
-- | Bounded Fields
-- BoundedField ensures that divide by zero works.  Having said this, actually printing one/zero as `Infinity` and -one/zero as `-Infinity` ain't coming from tower.
--
-- >>> one/zero
-- Infinity
--
-- |
-- >>> -one/zero
-- -Infinity
--
-- |
-- >>> zero/zero+one
-- NaN
-- 
-- |
-- >>> logBase 2 4
-- 2.0
-- 
-- |
-- >>> 2 ** 2
-- 4.0
-- 
-- |
-- >>> sqrt 4
-- 2.0
-- 
-- |
-- >>> exp 2
-- 7.38905609893065
--
-- >>> log 2
-- 0.6931471805599453
--
-- * Vector Representable
--
-- | note no wrapping
-- >>> :set -XDataKinds
-- >>> let a = toV [1..3] :: V 3 Int
-- >>> a
-- V {toVector = [1,2,3]}
--
--
-- | 
--
-- >>> a+zero==a
-- True
-- >>> zero+a==a
-- True
-- >>> a+a
-- V {toVector = [2,4,6]}
--
-- |
-- >>> a-a == zero
-- True
--
-- |
-- >>> a * a
-- V {toVector = [1,4,9]}
--
-- |
-- >>> a `divMod` a
-- (V {toVector = [1,1,1]},V {toVector = [0,0,0]})
--
-- |
-- >>> let b = toV [1.0,2.0,3.0] :: V 3 Float
-- >>> b / b
-- V {toVector = [1.0,1.0,1.0]}
--
-- |
-- todo: not sure why the type is needed here
-- >>> :set -XFlexibleContexts
-- >>> let a = toV [3.0,2.0,1.0] :: V 3 Float
-- >>> size a :: Float
-- 3.7416575
--
-- |
-- >>> distance a b :: Float
-- 2.828427
--
--
-- |
-- >>> a >< b
-- V {toVector = [V {toVector = [3.0,6.0,9.0]},V {toVector = [2.0,4.0,6.0]},V {toVector = [1.0,2.0,3.0]}]}
--
--
-- |
-- >>> a <.> b :: Float
-- 10.0
--
-- * Matrix Double Representable - (r (r a))
--
-- | note no wrapping
-- >>> :set -XDataKinds
-- >>> let a = toV [1..3] :: V 3 Int
-- >>> a
-- V {toVector = [1,2,3]}
--
--
-- | 
--
-- >>> a+zero==a
-- True
-- >>> zero+a==a
-- True
-- >>> a+a
-- V {toVector = [2,4,6]}
--
-- |
-- >>> a-a == zero
-- True
--
-- |
-- >>> a * a
-- V {toVector = [1,4,9]}
--
-- |
-- >>> a `divMod` a
-- (V {toVector = [1,1,1]},V {toVector = [0,0,0]})
--
-- |
-- >>> let b = toV [1.0,2.0,3.0] :: V 3 Float
-- >>> b / b
-- V {toVector = [1.0,1.0,1.0]}
--
-- |
-- todo: not sure why the type is needed here
-- >>> :set -XFlexibleContexts
-- >>> let a = toV [3.0,2.0,1.0] :: V 3 Float
-- >>> size a :: Float
-- 3.7416575
--
-- |
-- >>> distance a b :: Float
-- 2.828427
--
--
-- |
-- >>> a >< b
-- V {toVector = [V {toVector = [3.0,6.0,9.0]},V {toVector = [2.0,4.0,6.0]},V {toVector = [1.0,2.0,3.0]}]}
--
--
-- |
-- >>> a <.> b :: Float
-- 10.0
--



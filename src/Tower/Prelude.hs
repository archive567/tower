{-# OPTIONS_GHC -Wall #-}

-- | exactly protolude hiding tower api
module Tower.Prelude (module X) where

import Protolude as X hiding
    ( (+)
    , (-)
    , (*)
    , (/)
    , zero
    , negate
    , recip
    , Integral(..)
    , round
    , ceiling
    , floor
    , (^^)
    , Semiring(..)
    , log
    , logBase
    , exp
    , sqrt
    , (**)
    , abs
    , (^)
    , infinity
    , Bounded(..)
    , isNaN
    , fromIntegral
    , toInteger
    , fromInteger
    )

import Tower.Algebra as X
import Tower.V as X
import Tower.M as X

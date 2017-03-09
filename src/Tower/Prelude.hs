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
    , truncate
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
    , fromIntegral
    , toInteger
    , fromInteger
    )

import Tower.Algebra as X
import Tower.VectorA as X
import Tower.VectorU as X

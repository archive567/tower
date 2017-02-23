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
    , Semiring(..)
    , log
    , logBase
    , exp
    , sqrt
    , (**)
    , abs
    , (^)
    , infinity
    )

import Tower.Algebra as X
import Tower.VectorA as X()
import Tower.VectorU as X
import Tower.Extrema as X

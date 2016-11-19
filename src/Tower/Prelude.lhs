> {-# OPTIONS_GHC -Wall #-}
> module Tower.Prelude (module X) where
>
> import Protolude as X hiding
>     ( Semigroup (..)
>     , Monoid(..)
>     , (+)
>     , (-)
>     , (*)
>     , (/)
>     , cancel
>     , zero
>     , one
>     , negate
>     , div
>     , mod
>     , rem
>     , quot
>     , Integral(..)
>     , Semiring(..)
>     , Real(..)
>     , Bounded(..)
>     , fromIntegral
>     , cos
>     , sin
>     , tan
>     , acos
>     , asin
>     , atan
>     , acosh
>     , asinh
>     , atanh
>     , cosh
>     , sinh
>     , tanh
>     , log
>     , logBase
>     , exp
>     , pi
>     , floor
>     , round
>     , ceiling
>     , truncate
>     , sqrt
>     , (**)
>     , (^^)
>     , isNaN
>     , fromRational
>     , fromInteger
>     , abs
>     , (^)
>     , infinity
>     )
> import Tower as X
> import Tower.Double as X

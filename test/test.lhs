> {-# OPTIONS_GHC -Wall #-}
> module Main where
>
> import qualified Protolude as P
> import Protolude (Eq(..), Bool(..), Int, Integer, Float, Double, Rational, ($))
> import Test.Tasty (TestTree, testGroup, defaultMain)
> import Test.Tasty.SmallCheck as SC
> import Test.Tasty.QuickCheck as QC
>
> import Tower
> 
> scheckInt :: TestTree
> scheckInt = testGroup "smallchecks - int"
>     [ SC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Int -> Int -> Int -> Bool)
>     , SC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Int -> Bool)
>     , SC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Int -> Bool)
>     , SC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Int -> Int -> Bool)
>     , SC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Int -> Int -> Bool)
>     , SC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Int -> Int -> Bool)
>     , SC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Int -> Bool)
>     , SC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Int -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Int -> Int -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Int -> Int -> Bool)
>     , SC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Int -> Int -> Int -> Bool)
>     , SC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Int -> Int -> Bool)
>     , SC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Int -> Bool)
>     , SC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Int -> Int -> Int -> Bool)
>     , SC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Int -> Int -> Int -> Bool)
>     , SC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Int -> Bool)
>     , SC.testProperty "integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a" $ 
>       ((\a b -> b == zero P.|| b * (a `div` b) + (a `mod` b) == a) :: Int -> Int -> Bool)
>     , SC.testProperty "integral quotrem: b == zero || b * (a `quot` b) + (a `rem` b) == a" $ 
>       ((\a b -> b == zero P.|| b * (a `quot` b) + (a `rem` b) == a) :: Int -> Int -> Bool)
>     ]
>
> scheckInteger :: TestTree
> scheckInteger = testGroup "smallchecks - integer"
>     [ SC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Integer -> Integer -> Integer -> Bool)
>     , SC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Integer -> Bool)
>     , SC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Integer -> Bool)
>     , SC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Integer -> Integer -> Bool)
>     , SC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Integer -> Integer -> Bool)
>     , SC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Integer -> Integer -> Bool)
>     , SC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Integer -> Bool)
>     , SC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Integer -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Integer -> Integer -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Integer -> Integer -> Bool)
>     , SC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Integer -> Integer -> Integer -> Bool)
>     , SC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Integer -> Integer -> Bool)
>     , SC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Integer -> Bool)
>     , SC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Integer -> Integer -> Integer -> Bool)
>     , SC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Integer -> Integer -> Integer -> Bool)
>     , SC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Integer -> Bool)
>     , SC.testProperty "integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a" $ 
>       ((\a b -> b == zero P.|| b * (a `div` b) + (a `mod` b) == a) :: Integer -> Integer -> Bool)
>     , SC.testProperty "integral quotrem: b == zero || b * (a `quot` b) + (a `rem` b) == a" $ 
>       ((\a b -> b == zero P.|| b * (a `quot` b) + (a `rem` b) == a) :: Integer -> Integer -> Bool)
>     ]
>
> scheckFloat :: TestTree
> scheckFloat = testGroup "smallchecks - float"
>     [ SC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Float -> Float -> Float -> Bool)
>     , SC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Float -> Bool)
>     , SC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Float -> Bool)
>     , SC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Float -> Float -> Bool)
>     , SC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Float -> Float -> Bool)
>     , SC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Float -> Float -> Bool)
>     , SC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Float -> Bool)
>     , SC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Float -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Float -> Float -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Float -> Float -> Bool)
>     , SC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Float -> Float -> Float -> Bool)
>     , SC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Float -> Float -> Bool)
>     , SC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Float -> Bool)
>     , SC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Float -> Float -> Float -> Bool)
>     , SC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Float -> Float -> Float -> Bool)
>     , SC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Float -> Bool)
>     ]
>     
> scheckDouble :: TestTree
> scheckDouble = testGroup "smallchecks - double"
>     [ SC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Double -> Double -> Double -> Bool)
>     , SC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Double -> Bool)
>     , SC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Double -> Bool)
>     , SC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Double -> Double -> Bool)
>     , SC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Double -> Double -> Bool)
>     , SC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Double -> Double -> Bool)
>     , SC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Double -> Bool)
>     , SC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Double -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Double -> Double -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Double -> Double -> Bool)
>     , SC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Double -> Double -> Double -> Bool)
>     , SC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Double -> Double -> Bool)
>     , SC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Double -> Bool)
>     , SC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Double -> Double -> Double -> Bool)
>     , SC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Double -> Double -> Double -> Bool)
>     , SC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Double -> Bool)
>     ]
>     
> scheckRational :: TestTree
> scheckRational = testGroup "smallchecks - rational"
>     [ SC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Rational -> Rational -> Rational -> Bool)
>     , SC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Rational -> Bool)
>     , SC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Rational -> Bool)
>     , SC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Rational -> Rational -> Bool)
>     , SC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Rational -> Rational -> Bool)
>     , SC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Rational -> Rational -> Bool)
>     , SC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Rational -> Bool)
>     , SC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Rational -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Rational -> Rational -> Bool)
>     , SC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Rational -> Rational -> Bool)
>     , SC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Rational -> Rational -> Rational -> Bool)
>     , SC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Rational -> Rational -> Bool)
>     , SC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Rational -> Bool)
>     , SC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Rational -> Rational -> Rational -> Bool)
>     , SC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Rational -> Rational -> Rational -> Bool)
>     , SC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Rational -> Bool)
>     ]

> qcheckInt :: TestTree
> qcheckInt = testGroup "quickchecks - int"
>     [ QC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Int -> Int -> Int -> Bool)
>     , QC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Int -> Bool)
>     , QC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Int -> Bool)
>     , QC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Int -> Int -> Bool)
>     , QC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Int -> Int -> Bool)
>     , QC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Int -> Int -> Bool)
>     , QC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Int -> Bool)
>     , QC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Int -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Int -> Int -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Int -> Int -> Bool)
>     , QC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Int -> Int -> Int -> Bool)
>     , QC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Int -> Int -> Bool)
>     , QC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Int -> Bool)
>     , QC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Int -> Int -> Int -> Bool)
>     , QC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Int -> Int -> Int -> Bool)
>     , QC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Int -> Bool)
>     , QC.testProperty "integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a" $ 
>       ((\a b -> b == zero P.|| b * (a `div` b) + (a `mod` b) == a) :: Int -> Int -> Bool)
>     , QC.testProperty "integral quotrem: b == zero || b * (a `quot` b) + (a `rem` b) == a" $ 
>       ((\a b -> b == zero P.|| b * (a `quot` b) + (a `rem` b) == a) :: Int -> Int -> Bool)
>     ]
>
> qcheckInteger :: TestTree
> qcheckInteger = testGroup "quickchecks - integer"
>     [ QC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Integer -> Integer -> Integer -> Bool)
>     , QC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Integer -> Bool)
>     , QC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Integer -> Bool)
>     , QC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Integer -> Integer -> Bool)
>     , QC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Integer -> Integer -> Bool)
>     , QC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Integer -> Integer -> Bool)
>     , QC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Integer -> Bool)
>     , QC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Integer -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Integer -> Integer -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Integer -> Integer -> Bool)
>     , QC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Integer -> Integer -> Integer -> Bool)
>     , QC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Integer -> Integer -> Bool)
>     , QC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Integer -> Bool)
>     , QC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Integer -> Integer -> Integer -> Bool)
>     , QC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Integer -> Integer -> Integer -> Bool)
>     , QC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Integer -> Bool)
>     , QC.testProperty "integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a" $ 
>       ((\a b -> b == zero P.|| b * (a `div` b) + (a `mod` b) == a) :: Integer -> Integer -> Bool)
>     , QC.testProperty "integral quotrem: b == zero || b * (a `quot` b) + (a `rem` b) == a" $ 
>       ((\a b -> b == zero P.|| b * (a `quot` b) + (a `rem` b) == a) :: Integer -> Integer -> Bool)
>     ]
>     
> qcheckFloat :: TestTree
> qcheckFloat = testGroup "quickchecks - float"
>     [ QC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Float -> Float -> Float -> Bool)
>     , QC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Float -> Bool)
>     , QC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Float -> Bool)
>     , QC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Float -> Float -> Bool)
>     , QC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Float -> Float -> Bool)
>     , QC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Float -> Float -> Bool)
>     , QC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Float -> Bool)
>     , QC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Float -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Float -> Float -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Float -> Float -> Bool)
>     , QC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Float -> Float -> Float -> Bool)
>     , QC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Float -> Float -> Bool)
>     , QC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Float -> Bool)
>     , QC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Float -> Float -> Float -> Bool)
>     , QC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Float -> Float -> Float -> Bool)
>     , QC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Float -> Bool)
>     ]
>     
> qcheckDouble :: TestTree
> qcheckDouble = testGroup "quickchecks - double"
>     [ QC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Double -> Double -> Double -> Bool)
>     , QC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Double -> Bool)
>     , QC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Double -> Bool)
>     , QC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Double -> Double -> Bool)
>     , QC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Double -> Double -> Bool)
>     , QC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Double -> Double -> Bool)
>     , QC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Double -> Bool)
>     , QC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Double -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Double -> Double -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Double -> Double -> Bool)
>     , QC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Double -> Double -> Double -> Bool)
>     , QC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Double -> Double -> Bool)
>     , QC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Double -> Bool)
>     , QC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Double -> Double -> Double -> Bool)
>     , QC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Double -> Double -> Double -> Bool)
>     , QC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Double -> Bool)
>     ]
>     
> qcheckRational :: TestTree
> qcheckRational = testGroup "quickchecks - rational"
>     [ QC.testProperty "semigroup: a + b = b + a" $
>       ((\a b c -> (a + b) + c == a + (b + c)) :: Rational -> Rational -> Rational -> Bool)
>     , QC.testProperty "monoid leftid: zero + a = a" $
>       ((\a -> zero + a == a) :: Rational -> Bool)
>     , QC.testProperty "monoid rightid: a + zero = a" $
>       ((\a -> a + zero == a) :: Rational -> Bool)
>     , QC.testProperty "cancellative rightminus1: (a + b) - b = a" $
>       ((\a b -> (a + b) - b == a) :: Rational -> Rational -> Bool)
>     , QC.testProperty "cancellative rightminus2: a + (b - b) = a" $
>       ((\a b -> a + (b - b) == a) :: Rational -> Rational -> Bool)
>     , QC.testProperty "group negateminus: a + negate b == a - b" $
>       ((\a b -> a + negate b == a - b) :: Rational -> Rational -> Bool)
>     , QC.testProperty "group leftinverse: negate a + a == zero" $
>       ((\a -> negate a + a == zero) :: Rational -> Bool)
>     , QC.testProperty "group rightinverse: a + negate a == zero" $
>       ((\a -> a + negate a == zero) :: Rational -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Rational -> Rational -> Bool)
>     , QC.testProperty "abelian commutative: a + b == b + a" $
>       ((\a b -> a + b == b + a) :: Rational -> Rational -> Bool)
>     , QC.testProperty "rg times associativity: (a * b) * c == a * (b * c)" $
>       ((\a b c -> (a * b) * c == a * (b * c)) :: Rational -> Rational -> Rational -> Bool)
>     , QC.testProperty "rg times commutivity: a * b == b * a" $
>       ((\a b -> a * b == b * a) :: Rational -> Rational -> Bool)
>     , QC.testProperty "rg annihilation: a * zero == zero" $
>       ((\a -> a * zero == zero) :: Rational -> Bool)
>     , QC.testProperty "rg left distributivity: a * (b + c) == a * b + a * c" $
>       ((\a b c -> a * (b + c) == a * b + a * c) :: Rational -> Rational -> Rational -> Bool)
>     , QC.testProperty "rg right distributivity: (a + b) * c == a * c + b * c" $
>       ((\a b c -> (a + b) * c == a * c + b * c) :: Rational -> Rational -> Rational -> Bool)
>     , QC.testProperty "rig times id: a * one == a && one * a == a" $
>       ((\a -> a * one == a P.&& one * a == a) :: Rational -> Bool)
>     ]

> tests :: TestTree
> tests = testGroup "Tower"
>     [ qcheckInt
>     , qcheckInteger
>     , qcheckFloat
>     , qcheckDouble
>     , qcheckRational
>     , scheckInt
>     , scheckInteger
>     , scheckFloat
>     , scheckDouble
>     , scheckRational
>     ]
> 
> main :: P.IO ()
> main = do
>     defaultMain tests
>






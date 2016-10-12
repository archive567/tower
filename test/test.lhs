> {-# OPTIONS_GHC -Wall #-}
> module Main where
>
> import Protolude hiding ((+),(-),(*),(/),zero,one,negate,div,mod,rem,quot, Integral(..))
> import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
> import Test.Tasty.QuickCheck
> import Tower
>
> data LawArity a =
>     Unary (a -> Bool) |
>     Binary (a -> a -> Bool) |
>     Ternary (a -> a -> a -> Bool) |
>     Ornary (a -> a -> a -> a -> Bool)
>
> type Law a = (TestName, (LawArity a))
>
> testLawOf  :: (Arbitrary a, Show a) => [a] -> Law a -> TestTree
> testLawOf _ (name, Unary f) = testProperty name f
> testLawOf _ (name, Binary f) = testProperty name f
> testLawOf _ (name, Ternary f) = testProperty name f
> testLawOf _ (name, Ornary f) = testProperty name f
>
> tests :: TestTree
> tests = testGroup "everything" $
>     [ testGroup "Int - Ring" $ testLawOf ([]::[Int]) <$> ringLaws
>     , testGroup "Int - Integral" $ testLawOf ([]::[Int]) <$> integralLaws
>     , testGroup "Integer - Ring" $ testLawOf ([]::[Integer]) <$> ringLaws
>     , testGroup "Integer - Integral" $ testLawOf ([]::[Integer]) <$> integralLaws
>     , testGroup "Float - Ring" $ testLawOf ([]::[Float]) <$> ringLaws
>     , testGroup "Double - Ring" $ testLawOf ([]::[Double]) <$> ringLaws
>     , testGroup "Rational - Ring" $ testLawOf ([]::[Rational]) <$> ringLaws
>     ]
>
> main :: IO ()
> main = defaultMain tests
>
> ringLaws ::
>     ( Eq a
>     , Group a
>     , Rig a
>     , Ring a
>     ) => [Law a]
> ringLaws =
>     [ ("semigroup: a + b = b + a", Ternary (\a b c -> (a + b) + c == a + (b + c)))
>     , ("monoid leftid: zero + a = a", Unary (\a -> zero + a == a))
>     , ("monoid rightid: a + zero = a", Unary (\a -> a + zero == a))
>     , ("cancellative rightminus1: (a + b) - b = a", Binary (\a b -> (a + b) - b == a))
>     , ("cancellative rightminus2: a + (b - b) = a", Binary (\a b -> a + (b - b) == a))
>     , ("group negateminus: a + negate b == a - b", Binary (\a b -> a + negate b == a - b))
>     , ("group leftinverse: negate a + a == zero", Unary (\a -> negate a + a == zero))
>     , ("group rightinverse: a + negate a == zero", Unary (\a -> a + negate a == zero))
>     , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
>     , ("rg times associativity: (a * b) * c == a * (b * c)", Ternary (\a b c -> (a * b) * c == a * (b * c)))
>     , ("rg times commutivity: a * b == b * a", Binary (\a b -> a * b == b * a))
>     , ("rg annihilation: a * zero == zero", Unary (\a -> a * zero == zero))
>     , ("rg left distributivity: a * (b + c) == a * b + a * c", Ternary (\a b c -> a * (b + c) == a * b + a * c))
>     , ("rg right distributivity: (a + b) * c == a * c + b * c", Ternary (\a b c -> (a + b) * c == a * c + b * c))
>     , ("rig times id: a * one == a && one * a == a", Unary (\a -> a * one == a && one * a == a))
>     ]
>
> integralLaws ::
>     ( Eq a
>     , Rig a
>     , Integral a
>     ) => [Law a]
> integralLaws =
>     [ ("integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a", Binary (\a b -> b == zero || b * (a `div` b) + (a `mod` b) == a))
>     , ("integral quotrem: b == zero || b * (a `quot` b) + (a `rem` b) == a", Binary (\a b -> b == zero || b * (a `quot` b) + (a `rem` b) == a))
>    ]
>
>

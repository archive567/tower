{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Protolude hiding ((+),(-),(*),(/),zero,one,negate,recip,div,mod,rem,quot, Integral(..))
import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck
import Tower
import Tower.Double()
import Tower.Float()
import Tower.Int()
import Tower.UVector

data LawArity a =
    Unary (a -> Bool) |
    Binary (a -> a -> Bool) |
    Ternary (a -> a -> a -> Bool) |
    Ornary (a -> a -> a -> a -> Bool)

type Law a = (TestName, LawArity a)

testLawOf  :: (Arbitrary a, Show a) => [a] -> Law a -> TestTree
testLawOf _ (name, Unary f) = testProperty name f
testLawOf _ (name, Binary f) = testProperty name f
testLawOf _ (name, Ternary f) = testProperty name f
testLawOf _ (name, Ornary f) = testProperty name f

tests :: TestTree
tests = testGroup "everything"
    [ testGroup "Int - Additive" $ testLawOf ([]::[Int]) <$> additiveLaws
    , testGroup "Int - Additive Group" $ testLawOf ([]::[Int]) <$> additiveGroupLaws
    , testGroup "Int - Multiplicative" $ testLawOf ([]::[Int]) <$> multiplicativeLaws
    , testGroup "Int - Distributive" $ testLawOf ([]::[Int]) <$> distributiveLaws
    , testGroup "Int - Integral" $ testLawOf ([]::[Int]) <$> integralLaws
    , testGroup "Float - Additive" $ testLawOf ([]::[Float]) <$> additiveLaws
    , testGroup "Float - Additive Group" $ testLawOf ([]::[Float]) <$> additiveGroupLaws
    , testGroup "Float - Multiplicative" $ testLawOf ([]::[Float]) <$> multiplicativeLaws
    , testGroup "Float - Field" $ testLawOf ([]::[Float]) <$> fieldLaws
    , testGroup "Float - Distributive" $ testLawOf ([]::[Float]) <$> distributiveLaws
    , testGroup "Float - Ring" $ testLawOf ([]::[Float]) <$> ringLaws
    , testGroup "UVector 5 Int - Additive" $ testLawOf ([]::[UVector 5 Int]) <$> additiveLaws
    , testGroup "UVector 5 Int - Additive Group" $ testLawOf ([]::[UVector 5 Int]) <$> additiveGroupLaws
    , testGroup "UVector 5 Int - Multiplicative" $ testLawOf ([]::[UVector 5 Int]) <$> multiplicativeLaws
    , testGroup "UVector 5 Int - Distributive" $ testLawOf ([]::[UVector 5 Int]) <$> distributiveLaws
    -- , testGroup "UVector 5 Int - Integral" $ testLawOf ([]::[UVector 5 Int]) <$> integralLaws
    , testGroup "UVector 5 Float - Additive" $ testLawOf ([]::[UVector 5 Float]) <$> additiveLaws
    , testGroup "UVector 5 Float - Additive Group" $ testLawOf ([]::[UVector 5 Float]) <$> additiveGroupLaws
    , testGroup "UVector 5 Float - Multiplicative" $ testLawOf ([]::[UVector 5 Float]) <$> multiplicativeLaws
    , testGroup "UVector 5 Float - Field" $ testLawOf ([]::[UVector 5 Float]) <$> fieldLaws
    , testGroup "UVector 5 Float - Distributive" $ testLawOf ([]::[UVector 5 Float]) <$> distributiveLaws
    , testGroup "UVector 5 Float - Ring" $ testLawOf ([]::[UVector 5 Float]) <$> ringLaws
    ]

main :: IO ()
main = defaultMain tests

additiveLaws ::
    ( Eq a
    , Additive a
    ) => [Law a]
additiveLaws =
    [ ("associative: a + b = b + a", Ternary (\a b c -> (a + b) + c == a + (b + c)))
    , ("left id: zero + a = a", Unary (\a -> zero + a == a))
    , ("right id: a + zero = a", Unary (\a -> a + zero == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    ]

additiveGroupLaws ::
    ( Eq a
    , AdditiveGroup a
    ) => [Law a]
additiveGroupLaws =
    [ ("minus: a - a = zero", Unary (\a -> (a - a) == zero))
    , ("negate minus: negate a == zero - a", Unary (\a -> negate a == zero - a))
    , ("negate left: negate a + a == zero", Unary (\a -> negate a + a == zero))
    , ("negate right: a + negate a == zero", Unary (\a -> a + negate a == zero))
    ]

multiplicativeLaws ::
    ( Eq a
    , Multiplicative a
    ) => [Law a]
multiplicativeLaws =
    [ ("associative: a * b = b * a", Ternary (\a b c -> (a * b) * c == a * (b * c)))
    , ("left id: one * a = a", Unary (\a -> one * a == a))
    , ("right id: a * one = a", Unary (\a -> a * one == a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    ]

fieldLaws ::
    ( Eq a
    , Field a
    ) => [Law a]
fieldLaws =
    [ ("divide: a / a = one", Unary (\a -> (a / a) == one))
    , ("recip divide: recip a == one / a", Unary (\a -> recip a == one / a))
    , ("recip left: recip a * a == one", Unary (\a -> recip a * a == one))
    , ("recip right: a * recip a == one", Unary (\a -> a * recip a == one))
    ]

distributiveLaws ::
    ( Eq a
    , Distributive a
    ) => [Law a]
distributiveLaws =
    [ ("annihilation: a * zero == zero", Unary (\a -> a * zero == zero))
    , ("left distributivity: a * (b + c) == a * b + a * c", Ternary (\a b c -> a * (b + c) == a * b + a * c))
    , ("right distributivity: (a + b) * c == a * c + b * c", Ternary (\a b c -> (a + b) * c == a * c + b * c))
    ]

ringLaws ::
    ( Eq a
    , Ring a
    ) => [Law a]
ringLaws =
    [
    ]

integralLaws ::
    ( Eq a
    , Integral a
    ) => [Law a]
integralLaws =
    [ ("integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a", Binary (\a b -> b == zero || b * (a `div` b) + (a `mod` b) == a))
    , ("integral quotrem: b == zero || b * (a `quot` b) + (a `rem` b) == a", Binary (\a b -> b == zero || b * (a `quot` b) + (a `rem` b) == a))
   ]

todoLaws ::
    ( Eq a
    -- , AdditiveModule s a
    ) => [Law a]
todoLaws =
    [ -- (a1+a2) +. s == a1 +. a2 +. s
      -- m2 s = s *. (m1 + m2) == s*.m1 + s*.m2
      -- m s1 s2 = (s1+s2)*.m == s1*.m + s2*.m
      -- s1 s2 = s1*.(s2*.m) == (s1*s2)*.m
      -- m = 1 *. m == m
      -- free modules
      -- m1 m2 = m1.*.m2 == m2.*.m1
      -- m1 m2 m3 = m1.*.(m2.*.m3) == (m1.*.m2).*.m3
      -- m = m == m.*.ones
      -- Banach
      -- v1 v2 = size (v1 - v2) == distance v1 v2
      -- v = isZero v || size (normalize v) == 1
      -- Metric
      -- v1 v2 = distance v1 v2 >= 0
      -- v1 v2 = v1 == v2 == distance v1 v2 == 0
      -- v1 v2 = distance v1 v2 == distance v2 v1
      -- m1 m2 m3 = distance m1 m2 <= distance m1 m3 + distance m2 m3
      --         && distance m1 m3 <= distance m1 m2 + distance m2 m3
      --         && distance m2 m3 <= distance m1 m3 + distance m2 m1
    ]


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Protolude hiding ((+),(-),(*),(/),zero,one,negate,recip,div,mod,rem,quot, Integral(..))
import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck
import Tower.Algebra
import Tower.VectorU
import Tower.VectorA

data LawArity a =
    Unary (a -> Bool) |
    Binary (a -> a -> Bool) |
    Ternary (a -> a -> a -> Bool) |
    Ornary (a -> a -> a -> a -> Bool) |
    Uniary (a -> Property)

type Law a = (TestName, LawArity a)

testLawOf  :: (Arbitrary a, Show a) => [a] -> Law a -> TestTree
testLawOf _ (name, Unary f) = testProperty name f
testLawOf _ (name, Binary f) = testProperty name f
testLawOf _ (name, Ternary f) = testProperty name f
testLawOf _ (name, Ornary f) = testProperty name f
testLawOf _ (name, Uniary f) = testProperty name f

tests :: TestTree
tests = testGroup "everything"
    [ testGroup "Int - Additive" $ testLawOf ([]::[Int]) <$> additiveLaws
    , testGroup "Int - Additive Group" $ testLawOf ([]::[Int]) <$> additiveGroupLaws
    , testGroup "Int - Multiplicative" $ testLawOf ([]::[Int]) <$> multiplicativeLaws
    , testGroup "Int - Distributive" $ testLawOf ([]::[Int]) <$> distributiveLaws
    , testGroup "Int - Integral" $ testLawOf ([]::[Int]) <$> integralLaws
    , testGroup "Float - Additive" $ testLawOf ([]::[Float]) <$> additiveFloatLaws
    , testGroup "Float - Additive Group" $ testLawOf ([]::[Float]) <$> additiveGroupLaws
    , testGroup "Float - Multiplicative" $ testLawOf ([]::[Float]) <$> multiplicativeFloatLaws
    , testGroup "Float - MultiplicativeGroup" $ testLawOf ([]::[Float]) <$> fieldFloatLaws
    , testGroup "Float - Distributive" $ testLawOf ([]::[Float]) <$> distributiveFloatLaws
    , testGroup "UVector 5 Int - Additive" $ testLawOf ([]::[VectorU 5 Int]) <$> additiveLaws
    , testGroup "VectorU 5 Int - Additive Group" $ testLawOf ([]::[VectorU 5 Int]) <$> additiveGroupLaws
    , testGroup "VectorU 5 Int - Multiplicative" $ testLawOf ([]::[VectorU 5 Int]) <$> multiplicativeLaws
    , testGroup "VectorU 5 Int - Distributive" $ testLawOf ([]::[VectorU 5 Int]) <$> distributiveLaws
    -- , testGroup "VectorU 5 Int - Integral" $ testLawOf ([]::[VectorU 5 Int]) <$> integralLaws
    , testGroup "VectorU 5 Float - Additive" $ testLawOf ([]::[VectorU 5 Float]) <$> additiveFloatLaws
    , testGroup "VectorU 5 Float - Additive Group" $ testLawOf ([]::[VectorU 5 Float]) <$> additiveGroupLaws
    , testGroup "VectorU 5 Float - Multiplicative" $ testLawOf ([]::[VectorU 5 Float]) <$> multiplicativeFloatLaws
    , testGroup "VectorU 5 Float - MultiplicativeGroup" $ testLawOf ([]::[VectorU 5 Float]) <$> fieldFloatLaws
    , testGroup "VectorU 5 Float - Distributive" $ testLawOf ([]::[VectorU 5 Float]) <$> distributiveFloatLaws
    , testGroup "VectorA Int - Additive" $ testLawOf ([]::[VectorA 5 [] Int]) <$> additiveLaws
    , testGroup "VectorA Int - Additive Group" $ testLawOf ([]::[VectorA 5 [] Int]) <$> additiveGroupLaws
    , testGroup "VectorA Int - Multiplicative" $ testLawOf ([]::[VectorA 5 [] Int]) <$> multiplicativeLaws
    , testGroup "VectorA Int - Distributive" $ testLawOf ([]::[VectorA 5 [] Int]) <$> distributiveLaws
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

additiveFloatLaws ::
    ( Eq a
    , Additive a
    , Show a
    , Arbitrary a
    ) => [Law a]
additiveFloatLaws =
    [ ("associative: a + b = b + a", Uniary $ expectFailure . (\a b c -> (a + b) + c == a + (b + c)))
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
    , ("negate cancel: negate a + a == zero", Unary (\a -> negate a + a == zero))
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


aboutEqual :: (AdditiveGroup a, Ord a) => a -> a -> a -> Bool
aboutEqual eps a b = a - b <= eps && b - a <= eps

multiplicativeFloatLaws ::
    ( Eq a
    , Show a
    , Arbitrary a
    , Multiplicative a
    ) => [Law a]
multiplicativeFloatLaws =
    [ ("associative: a * b = b * a", Uniary $ expectFailure .  (\a b c -> (a * b) * c == a * (b * c)))
    , ("left id: one * a = a", Unary (\a -> one * a == a))
    , ("right id: a * one = a", Unary (\a -> a * one == a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    ]

fieldLaws ::
    ( Eq a
    , MultiplicativeGroup a
    , AdditiveGroup a
    ) => [Law a]
fieldLaws =
    [ ("divide: a == zero || a / a = one", Unary (\a -> a == zero || (a / a) == one))
    , ("recip divide: recip a == one / a", Unary (\a -> recip a == one / a))
    , ("recip left: recip a * a == one", Unary (\a -> a == zero || recip a * a == one))
    , ("recip right: a * recip a == one", Unary (\a -> a == zero || a * recip a == one))
    ]

fieldFloatLaws ::
    ( MultiplicativeGroup a
    , AdditiveGroup a
    , Eq a
    ) => [Law a]
fieldFloatLaws =
    [ ("divide: a == zero || a / a == one", Uniary $ expectFailure . (\a -> a == zero || a / a == one))
    , ("recip divide: recip a == one / a", Unary (\a -> recip a == one / a))
    , ("recip left: recip a * a == one", Uniary $ expectFailure . (\a -> a == zero || recip a * a == one))
    , ("recip right: a * recip a == one", Uniary $ expectFailure . (\a -> a == zero || a * recip a == one))
    ]

distributiveLaws ::
    ( Eq a
    , Distributive a
    , Multiplicative a
    ) => [Law a]
distributiveLaws =
    [ ("annihilation: a * zero == zero", Unary (\a -> a * zero == zero))
    , ("left distributivity: a * (b + c) == a * b + a * c", Ternary (\a b c -> a * (b + c) == a * b + a * c))
    , ("right distributivity: (a + b) * c == a * c + b * c", Ternary (\a b c -> (a + b) * c == a * c + b * c))
    ]

distributiveFloatLaws ::
    ( Eq a
    , Distributive a
    , Multiplicative a
    , Show a
    , Arbitrary a
    ) => [Law a]
distributiveFloatLaws =
    [ ("annihilation: a * zero == zero", Unary (\a -> a * zero == zero))
    , ("left distributivity: a * (b + c) == a * b + a * c", Uniary $ expectFailure . (\a b c -> a * (b + c) == a * b + a * c))
    , ("right distributivity: (a + b) * c == a * c + b * c", Uniary $ expectFailure . (\a b c -> (a + b) * c == a * c + b * c))
    ]


integralLaws ::
    ( Eq a
    , Integral a
    ) => [Law a]
integralLaws =
    [ ("integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a", Binary (\a b -> b == zero || b * (a `div` b) + (a `mod` b) == a))
   ]

todoLaws ::
    ( -- Eq a
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


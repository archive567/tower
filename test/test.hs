{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Tower.Prelude

import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck
import Test.DocTest
-- import Test.QuickCheck

main :: IO ()
main = do
    doctest ["src/Tower/Examples.hs"]
    defaultMain tests

data LawArity a =
    Nonary Bool |
    Unary (a -> Bool) |
    Binary (a -> a -> Bool) |
    Ternary (a -> a -> a -> Bool) |
    Ornary (a -> a -> a -> a -> Bool) |
    Failiary (a -> Property)

type Law a = (TestName, LawArity a)

type Law2 a b = (TestName, LawArity a)

testLawOf  :: (Arbitrary a, Show a) => [a] -> Law a -> TestTree
testLawOf _ (name, Nonary f) = testProperty name f
testLawOf _ (name, Unary f) = testProperty name f
testLawOf _ (name, Binary f) = testProperty name f
testLawOf _ (name, Ternary f) = testProperty name f
testLawOf _ (name, Ornary f) = testProperty name f
testLawOf _ (name, Failiary f) = testProperty name f

tests :: TestTree
tests = testGroup "everything" [testsInt, testsFloat]

testsInt :: TestTree
testsInt = testGroup "Int"
    [ testGroup "Additive" $ testLawOf ([]::[Int]) <$>
      additiveLaws
    , testGroup "Additive Group" $ testLawOf ([]::[Int]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative" $ testLawOf ([]::[Int]) <$>
      multiplicativeLaws
    , testGroup "MultiplicativeGroup" $ testLawOf ([]::[Float]) <$>
      multiplicativeGroupLaws
    , testGroup "Distributive" $ testLawOf ([]::[Int])
      <$> distributiveLaws
    , testGroup "Integral" $ testLawOf ([]::[Int]) <$>
      integralLaws
    , testGroup "Signed" $ testLawOf ([]::[Int]) <$>
      signedLaws
    ]

testsFloat :: TestTree
testsFloat = testGroup "Float"
    [ testGroup "Additive - Associative Fail" $ testLawOf ([]::[Float]) <$>
      additiveLawsFail
    , testGroup "Additive Group" $ testLawOf ([]::[Float]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative - Associative Fail" $
      testLawOf ([]::[Float]) <$>
      multiplicativeLawsFail
    , testGroup "MultiplicativeGroup" $ testLawOf ([]::[Float]) <$>
      multiplicativeGroupLaws
    , testGroup "Distributive - Fail" $ testLawOf ([]::[Float]) <$>
      distributiveLawsFail
    , testGroup "Signed" $ testLawOf ([]::[Float]) <$>
      signedLaws
    , testGroup "Bounded Field" $ testLawOf ([]::[Float]) <$>
      boundedFieldLaws
    , testGroup "Metric" $ testLawOf ([]::[Float]) <$> metricLaws
    , testGroup "Quotient Field" $ testLawOf ([]::[Float]) <$>
      quotientFieldLaws
    , testGroup "Exponential Ring" $ testLawOf ([]::[Float]) <$> expRingLaws
    , testGroup "Exponential Field" $ testLawOf ([]::[Float]) <$> expFieldLaws
    ]

testsVInt :: Int -> TestTree
testsVInt n = testGroup "VInt"
    [ testGroup "Additive" $ testLawOf ([]::[Int]) <$>
      additiveLaws
    , testGroup "Additive Group" $ testLawOf ([]::[Int]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative" $ testLawOf ([]::[Int]) <$>
      multiplicativeLaws
    , testGroup "MultiplicativeGroup" $ testLawOf ([]::[Float]) <$>
      multiplicativeGroupLaws
    , testGroup "Distributive" $ testLawOf ([]::[Int])
      <$> distributiveLaws
    , testGroup "Integral" $ testLawOf ([]::[Int]) <$>
      integralLaws
    , testGroup "Signed" $ testLawOf ([]::[Int]) <$>
      signedLaws
    ]


additiveLaws ::
    ( Eq a
    , Additive a
    ) => [Law a]
additiveLaws =
    [ ( "associative: (a + b) + c = a + (b + c)"
      , Ternary (\a b c -> (a + b) + c == a + (b + c)))
    , ("left id: zero + a = a", Unary (\a -> zero + a == a))
    , ("right id: a + zero = a", Unary (\a -> a + zero == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    ]

additiveLawsApprox ::
    ( Additive a
    , Epsilon a
    ) => [Law a]
additiveLawsApprox =
    [ ( "associative: (a + b) + c ≈ a + (b + c)"
      , Ternary (\a b c -> (a + b) + c ≈ a + (b + c)))
    , ("left id: zero + a = a", Unary (\a -> zero + a == a))
    , ("right id: a + zero = a", Unary (\a -> a + zero == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    ]

additiveLawsFail ::
    ( Additive a
    , Epsilon a
    , Show a
    , Arbitrary a
    ) => [Law a]
additiveLawsFail =
    [ ( "associative: (a + b) + c ≈ a + (b + c)"
      , Failiary $ expectFailure . (\a b c -> (a + b) + c ≈ a + (b + c)))
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
    [ ( "associative: (a * b) * c = a * (b * c)"
      , Ternary (\a b c -> (a * b) * c == a * (b * c)))
    , ("left id: one * a = a", Unary (\a -> one * a == a))
    , ("right id: a * one = a", Unary (\a -> a * one == a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    ]

multiplicativeLawsApprox ::
    ( Epsilon a
    ) => [Law a]
multiplicativeLawsApprox =
    [ ("associative: (a * b) * c ≈ a * (b * c)"
      , Ternary (\a b c -> (a * b) * c ≈ a * (b * c)))
    , ("left id: one * a = a", Unary (\a -> one * a == a))
    , ("right id: a * one = a", Unary (\a -> a * one == a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    ]

multiplicativeLawsFail ::
    ( Epsilon a
    , Show a
    , Arbitrary a
    ) => [Law a]
multiplicativeLawsFail =
    [ ("associative: (a * b) * c ≈ a * (b * c)"
      , Failiary $ expectFailure . (\a b c -> (a * b) * c ≈ a * (b * c)))
    , ("left id: one * a = a", Unary (\a -> one * a == a))
    , ("right id: a * one = a", Unary (\a -> a * one == a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    ]

multiplicativeGroupLaws ::
    ( Epsilon a
    , MultiplicativeGroup a
    ) => [Law a]
multiplicativeGroupLaws =
    [ ( "divide: a == zero || a / a ≈ one", Unary (\a -> a == zero || (a / a) ≈ one))
    , ( "recip divide: recip a == one / a", Unary (\a -> recip a == one / a))
    , ( "recip left: a == zero || recip a * a ≈ one"
      , Unary (\a -> a == zero || recip a * a ≈ one))
    , ( "recip right: a == zero || a * recip a ≈ one"
      , Unary (\a -> a == zero || a * recip a ≈ one))
    ]

distributiveLaws ::
    ( Epsilon a
    ) => [Law a]
distributiveLaws =
    [ ("annihilation: a * zero == zero", Unary (\a -> a `times` zero == zero))
    , ("left distributivity: a * (b + c) ≈ a * b + a * c"
      , Ternary (\a b c -> a `times` (b + c) ≈ a `times` b + a `times` c))
    , ("right distributivity: (a + b) * c ≈ a * c + b * c"
      , Ternary (\a b c -> (a + b) `times` c ≈ a `times` c + b `times` c))
    ]

distributiveLawsApprox ::
    ( Epsilon a
    ) => [Law a]
distributiveLawsApprox =
    [ ("annihilation: a * zero == zero", Unary (\a -> a `times` zero == zero))
    , ("left distributivity: a * (b + c) ≈ a * b + a * c"
      , Ternary (\a b c -> a `times` (b + c) ≈ a `times` b + a `times` c))
    , ("right distributivity: (a + b) * c ≈ a * c + b * c"
      , Ternary (\a b c -> (a + b) `times` c ≈ a `times` c + b `times` c))
    ]

distributiveLawsFail ::
    ( Show a
    , Arbitrary a
    , Epsilon a
    ) => [Law a]
distributiveLawsFail =
    [ ("annihilation: a * zero == zero", Unary (\a -> a `times` zero == zero))
    , ("left distributivity: a * (b + c) ≈ a * b + a * c"
    , Failiary $ expectFailure .
      (\a b c -> a `times` (b + c) ≈ a `times` b + a `times` c))
    , ("right distributivity: (a + b) * c ≈ a * c + b * c"
    , Failiary $ expectFailure . (\a b c -> (a + b) `times` c ≈ a `times` c + b `times` c))
    ]

signedLaws ::
    ( Epsilon a
    , Normed a a
    ) => [Law a]
signedLaws =
    [ ("sign a * abs a == a", Unary (\a -> sign a `times` abs a == a))
    ]

integralLaws ::
    ( Eq a
    , Integral a
    , FromInteger a
    , ToInteger a
    ) => [Law a]
integralLaws =
    [ ( "integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a"
      , Binary (\a b -> b == zero || b `times` (a `div` b) + (a `mod` b) == a))
    , ( "fromIntegral a = a"
      , Unary (\a -> fromIntegral a == a))
    ]

boundedFieldLaws ::
    ( Ord a
    , BoundedField a
    ) => [Law a]
boundedFieldLaws =
    [ ("infinity laws"
      , Unary (\a ->
                  ((one :: Float)/zero + infinity == infinity) &&
                  (infinity + a == infinity) &&
                  isNaN ((infinity :: Float) - infinity) &&
                  isNaN ((infinity :: Float) / infinity) &&
                  isNaN (nan + a) &&
                  (zero :: Float)/zero /= nan))
    ]

metricLaws ::
    ( Metric a Float
    , Fractional a
    , Signed a
    , Normed a a
    ) => [Law a]
metricLaws =
    [ ( "positive"
      , Binary (\a b -> (distance a b :: Float) >= zero))
    , ("zero if equal"
      , Unary (\a -> (distance a a :: Float) == zero))
    , ( "associative"
      , Binary (\a b -> (distance a b :: Float) ≈ (distance b a :: Float)))
    , ( "triangle rule - sum of distances > distance"
      , Ternary (\a b c ->
                   (abs a > 10.0) ||
                   (abs b > 10.0) ||
                   (abs c > 10.0) ||
                   kindaPositive (distance a c + distance b c - (distance a b :: Float)) &&
                   kindaPositive (distance a b + distance b c - (distance a c :: Float)) &&
                   kindaPositive (distance a b + distance a c - (distance b c :: Float))))
    ]

quotientFieldLaws ::
    ( Ord a
    , Field a
    , QuotientField a
    , FromInteger a
    ) => [Law a]
quotientFieldLaws =
    [ ("x-1 < floor <= x <= ceiling < x+1"
      , Unary (\a ->
                  ((a - one) < fromIntegral (floor a)) &&
                  (fromIntegral (floor a) <= a) &&
                  (a <= fromIntegral (ceiling a)) &&
                  (fromIntegral (ceiling a) < a + one)))
    , ("round == floor (x + 1/2)"
      , Unary (\a -> round a == floor (a + one/(one+one))
              ))
    ]

expRingLaws ::
    ( ExpRing a
    , Epsilon a
    ) => [Law a]
expRingLaws =
    [ ("for +ive b, a != 0,1: a ** logBase a b ≈ b"
      , Binary (\a b ->
                  ( not (prettyPositive b) ||
                    not (nearZero (a - zero)) ||
                    (a == one) ||
                    (a == zero && nearZero (logBase a b)) ||
                    (a ** logBase a b ≈ b))))
    ]

expFieldLaws ::
    ( ExpField a
    , Epsilon a
    , Fractional a
    ) => [Law a]
expFieldLaws =
    [ ("sqrt . (**2) ≈ id"
      , Unary (\a -> not (prettyPositive a) || (a > 10.0) ||
                    (sqrt . (**(one+one)) $ a) ≈ a &&
                    ((**(one+one)) . sqrt $ a) ≈ a))
    , ("log . exp ≈ id"
      , Unary (\a -> not (prettyPositive a) || (a > 10.0) ||
                    (log . exp $ a) ≈ a &&
                    (exp . log $ a) ≈ a))
    ]

todoLaws ::
    ( -- Eq a
    -- , AdditiveModule s a
    ) => [Law a]
todoLaws =
    [ -- basis
      -- (a1+a2) +. s == a1 +. a2 +. s
      -- m2 s = s *. (m1 + m2) == s*.m1 + s*.m2
      -- m s1 s2 = (s1+s2)*.m == s1*.m + s2*.m
      -- s1 s2 = s1*.(s2*.m) == (s1*s2)*.m
      -- m = 1 *. m == m
      -- module
      -- m1 m2 = m1.*.m2 == m2.*.m1
      -- m1 m2 m3 = m1.*.(m2.*.m3) == (m1.*.m2).*.m3
      -- m = m == m.*.ones
      -- Banach
      -- v = isZero v || size (normalize v) == 1
    ]

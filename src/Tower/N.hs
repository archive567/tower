{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | N-dimensional array represented by a list of Ints

module Tower.N where

import qualified Protolude as P
import Protolude
    (($), (<$>), Functor(..), Show, Eq(..), (.), Maybe(..), Int, reverse, foldr, fst, zipWith, scanr, drop, sum)
import GHC.TypeLits
import qualified Data.Vector as V
import Data.Functor.Rep
import Data.Distributive as D
import Tower.Additive
import Test.QuickCheck
import Tower.Multiplicative
import Tower.Integral
import Data.Singletons
import Data.Singletons.Prelude

newtype N r a = N { flatten :: V.Vector a }
    deriving (Functor, Show, Eq, P.Foldable)

shape :: forall a r. (SingI r) => N (r :: [Nat]) a -> [Int]
shape _ =
    case (sing :: Sing r) of
      SNil -> []
      (SCons x xs) -> fmap P.fromIntegral (fromSing x: fromSing xs)

ndim :: forall a (r :: [Nat]) t. SingI r => t -> N r a -> Int
ndim _ = P.length . shape

ind :: [Int] -> [Int] -> Int
ind ns xs = sum $ zipWith (*) xs (drop 1 $ scanr (*) 1 (reverse ns))

unfoldI :: forall t. Integral t => [t] -> t -> ([t], t)
unfoldI ns x =
    foldr
    (\a (acc,rem) -> let (d,m) = divMod rem a in (m:acc,d))
    ([],x)
    (P.reverse ns)

unind :: [Int] -> Int -> [Int]
unind ns x= fst $ unfoldI ns x

unindSafe :: [Int] -> Int -> Maybe [Int]
unindSafe ns x = case ov of
                   0 -> Just $ reverse l
                   _ -> Nothing
  where
    (l,ov) = unfoldI ns x

instance forall (r :: [Nat]). (SingI r) => Distributive (N r) where
    distribute f = N $ V.generate n
        $ \i -> fmap (\(N v) -> V.unsafeIndex v i) f
      where
        ns = case (sing :: Sing r) of
          SNil -> []
          (SCons x xs) -> fmap P.fromInteger (fromSing x: fromSing xs)
        n = P.foldr (*) one ns

instance forall (r :: [Nat]). (SingI r) => Representable (N r) where
    type Rep (N r) = [Int]
    tabulate f = N $ V.generate n (f . unind ns)
      where
        ns = case (sing :: Sing r) of
          SNil -> []
          (SCons x xs) -> fmap P.fromIntegral (fromSing x: fromSing xs)
        n = P.foldr (*) one ns
    index (N xs) rs = xs V.! ind ns rs
      where
        ns = case (sing :: Sing r) of
          SNil -> []
          (SCons x xs') -> fmap P.fromIntegral (fromSing x: fromSing xs')

fromList :: forall a (r :: [Nat]). (AdditiveUnital a, SingI r) => [a] -> N r a
fromList l = N $ V.fromList $ P.take n $ l P.++ P.repeat zero
  where
    ns = case (sing :: Sing r) of
           SNil -> []
           (SCons x xs) -> fmap P.fromInteger (fromSing x: fromSing xs)
    n = P.foldr (*) one ns

instance forall a (r :: [Nat]). (SingI r, Arbitrary a, AdditiveUnital a) => Arbitrary (N r a) where
    arbitrary = frequency
        [ (1, P.pure zero)
        , (9, fromList <$> vector n)
        ]
      where
        ns = case (sing :: Sing r) of
               SNil -> []
               (SCons x xs) -> fmap P.fromInteger (fromSing x: fromSing xs)
        n = P.foldr (*) one ns


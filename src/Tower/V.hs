{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A tower based on Representable

module Tower.V where

import qualified Protolude as P
import Protolude
    (($), (<$>), Functor(..), Show, Eq(..), Maybe(..))
-- import Data.Foldable (Foldable(..))
import Tower.Algebra as T
import GHC.TypeLits
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import qualified Data.Vector as V
import Data.Proxy (Proxy(..))
import Data.Functor.Rep
import Data.Distributive as D
import qualified Data.List as List

newtype V n a = V { toVector :: V.Vector a }
    deriving (Functor, Show, Eq, P.Foldable)

instance KnownNat n => D.Distributive (V n) where
  distribute f = V $ V.generate n $ \i -> fmap (\(V v) -> V.unsafeIndex v i) f
    where
      n = P.fromInteger $ natVal (Proxy :: Proxy n)

instance KnownNat n => Representable (V n) where
  type Rep (V n) = P.Int
  tabulate = V P.. V.generate n
    where
      n = P.fromInteger $ natVal (Proxy :: Proxy n)
  index (V xs) i = xs V.! i

-- | create a `V n a` supplying the length
toV :: forall a n. (AdditiveUnital a) => P.Int -> [a] -> P.Maybe (V n a)
toV x l =
    case someNatVal $ fromIntegral x of
      Nothing -> Nothing
      Just (SomeNat _) -> Just (V mkV :: V x a)
        where
          mkV = V.fromList $ P.take x (l P.++ P.repeat zero)

-- | create a `V n a` where the user specifies length at the type level
toVType :: forall a n . (AdditiveUnital a, KnownNat n) => [a] -> V n a
toVType l = V $ V.fromList $ P.take n $ l P.++ P.repeat zero
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

-- | create a double V
toVV ::
    forall a m n.
    ( AdditiveUnital a
    , AdditiveUnital (V n a)) =>
    (P.Int, P.Int) ->
    [a] ->
    Maybe (V m (V n a))
toVV (i,j) l =
    case someNatVal $ fromIntegral i of
      Nothing -> Nothing
      Just _ -> case allJust $ toV j <$> splitList of
        Nothing -> Nothing
        Just x1 -> case someNatVal $ fromIntegral j of
          Nothing -> Nothing
          Just _ -> toV i x1
  where
    allJust x = P.foldr acc Nothing x
    acc a x = case a of
      Nothing -> Nothing
      Just a0 -> case x of
        Nothing -> Just [a0]
        Just x0 -> Just $ x0 P.<> [a0]
    splitList =
        List.unfoldr
        (\b -> case List.splitAt j b of
                ([],_) -> P.Nothing
                x -> P.Just x) $
        l P.++ P.repeat zero

-- | create a double V
toVV0 ::
    forall a m n.
    ( AdditiveUnital a
    , AdditiveUnital (V n a)) =>
    (P.Int, P.Int) ->
    [a] ->
    Maybe (V m (V n a))
toVV0 (i,j) l =
    case someNatVal $ fromIntegral j of
      Nothing -> Nothing
      Just _ -> case allJust $ toV j <$> splitList of
        Nothing -> Nothing
        Just x1 -> case someNatVal $ fromIntegral i of
          Nothing -> Nothing
          Just _ -> toV i x1
  where
    allJust x = P.foldr acc Nothing x
    acc a x = case a of
      Nothing -> Nothing
      Just a0 -> case x of
        Nothing -> Just [a0]
        Just x0 -> Just $ x0 P.<> [a0]
    splitList =
        List.unfoldr
        (\b -> case List.splitAt j b of
                ([],_) -> P.Nothing
                x -> P.Just x) $
        l P.++ P.repeat zero

toVVType ::
    forall a m n.
    ( AdditiveUnital a
    , AdditiveUnital (V n a)
    , KnownNat m
    , KnownNat n) =>
    [a] -> V m (V n a)
toVVType l = toVType (toVType <$> splitList)
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)
    splitList =
        List.unfoldr
        (\b -> case List.splitAt n b of
                ([],_) -> P.Nothing
                x -> P.Just x) $
        l P.++ P.repeat zero


newtype T n = T { un :: P.Int } deriving (Show)

toT :: forall n. P.Int -> P.Maybe (T n)
toT x =
    case someNatVal $ fromIntegral x of
      Nothing -> Nothing
      Just (SomeNat (_ :: Proxy m)) -> Just (T x :: T x)


{-
toT0 :: forall n. P.Int -> T n
toT0 s = case toSing s of
  SomeSing (SNat :: Sing n) -> (T s :: T n)
-}

test1 =
    -- putStrLn "How many cats do you own?"
    let c = 5 :: P.Integer in
    case toSing c of
      SomeSing (SNat :: Sing n) -> (T 5 :: T k)

-- https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html

toTs :: forall n. P.Int -> [P.Maybe (T n)]
toTs x = P.replicate x (toT x)

newtype L n = L { unL :: [P.Int] } deriving (Show)

toL :: forall n. P.Int -> [P.Int] -> P.Maybe (L n)
toL x l =
    case someNatVal $ fromIntegral x of
      Nothing -> Nothing
      Just (SomeNat (_ :: Proxy m)) -> Just (L (P.take x (l P.++ P.repeat zero)) :: L x)

newtype LV n a = LV { unLV :: [a] } deriving (Show)

toLV :: forall a n. (AdditiveUnital a) => P.Int -> [a] -> P.Maybe (LV n a)
toLV x l =
    case someNatVal $ fromIntegral x of
      Nothing -> Nothing
      Just (SomeNat (_ :: Proxy m)) -> Just (LV (P.take x (l P.++ P.repeat zero)) :: LV x a)

newtype LVN n a = LVN { unLVN :: V.Vector a } deriving (Show)

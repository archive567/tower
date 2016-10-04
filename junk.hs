module Tower where


class Semigroup g where
    {-# MINIMAL (+) #-}

    {-# INLINE (+) #-}
    infixl 6 +
    (+) :: g -> g -> g
    (+) = mutable2immutable (+=)

law_Semigroup_associativity :: (Eq g, Semigroup g ) => g -> g -> g -> Logic g
law_Semigroup_associativity g1 g2 g3 = g1 + (g2 + g3) == (g1 + g2) + g3

defn_Semigroup_plusequal :: (Eq_ g, Semigroup g, IsMutable g) => g -> g -> Logic g
defn_Semigroup_plusequal = simpleMutableDefn (+=) (+)

-- | Measures the degree to which a Semigroup obeys the associative law.
--
-- FIXME: Less-than-perfect associativity should be formalized in the class laws somehow.
associator :: (Semigroup g, Metric g) => g -> g -> g -> Scalar g
associator g1 g2 g3 = distance ((g1+g2)+g3) (g1+(g2+g3))

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.
cycle :: Semigroup m => m -> m
cycle xs = xs' where xs' = xs + xs'

instance Semigroup Int      where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup Integer  where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup Float    where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup Double   where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup Rational where (+) = (P.+) ; {-# INLINE (+) #-}

instance Semigroup () where
    {-# INLINE (+) #-}
    ()+() = ()

instance Semigroup   b => Semigroup   (a -> b) where
    {-# INLINE (+) #-}
    f+g = \a -> f a + g a

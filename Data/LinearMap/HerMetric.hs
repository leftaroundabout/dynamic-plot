{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ConstraintKinds            #-}




module Data.LinearMap.HerMetric where
    

    

import Data.VectorSpace
import Data.LinearMap
import Data.Basis
import Data.MemoTrie

    
import Diagrams.Prelude



class (HasBasis v, InnerSpace v, RealFloat (Scalar v), HasTrie (Basis v))
    => HasMetric v where
  toScalar :: v -> Maybe (Scalar v)

instance HasMetric Double where
  toScalar = Just
instance (HasMetric v) => HasMetric (v,v) where
  toScalar _ = Nothing
instance HasMetric R2 where
  toScalar v | v==zeroV   = Just 0
             | otherwise  = Nothing


-- | 'HerMetric' is a portmanteau of /Hermitian/ and /metric/ (in the sense as used
--   in e.g. general relativity).
data HerMetric v = IsotropMetric (Scalar v)
                 | HerMetric (v :-* v)

instance HasMetric v => AdditiveGroup (HerMetric v) where
  zeroV = IsotropMetric 0
  negateV (IsotropMetric i) = IsotropMetric $ negate i
  negateV (HerMetric m) = HerMetric $ negateV m
  IsotropMetric a ^+^ IsotropMetric b = IsotropMetric $ a + b
  IsotropMetric a ^+^ HerMetric m = HerMetric $ idL ^* a ^+^ m
  HerMetric m ^+^ IsotropMetric a = HerMetric $ m ^+^ idL ^* a
  HerMetric m ^+^ HerMetric n = HerMetric $ m ^+^ n
instance HasMetric v => VectorSpace (HerMetric v) where
  type Scalar (HerMetric v) = Scalar v
  s *^ (IsotropMetric a) = IsotropMetric $ s * a
  s *^ (HerMetric m) = HerMetric $ s *^ m 

projector :: HasMetric v => v -> HerMetric v
projector v = case toScalar v of
   Nothing -> HerMetric (linear $ \u -> v ^* (u<.>v))
   Just s  -> IsotropMetric $ s*s


metric :: HasMetric v => HerMetric v -> v -> Scalar v
metric (HerMetric m) v = sqrt $ v <.> lapply m v

basisMetric :: HasMetric v => HerMetric v -> Basis v -> Scalar v
basisMetric (HerMetric m) b = sqrt $ basisValue b <.> atBasis m b


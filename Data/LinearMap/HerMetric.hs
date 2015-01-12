{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ConstraintKinds            #-}




module Data.LinearMap.HerMetric (
    HasMetric(..)
  , (^<.>)
  , HerMetric
  , projector
  , metricSq, metric
  , counterMetricSq, counterMetric
  ) where
    

    

import Data.VectorSpace
import Data.LinearMap
import Data.Basis
import Data.MemoTrie

    
import Diagrams.Prelude


infixr 7 <.>^, ^<.>

class ( HasBasis v, RealFloat (Scalar v), HasTrie (Basis v)
      , VectorSpace (DualSpace v), HasBasis (DualSpace v)
      , Scalar v ~ Scalar (DualSpace v), Basis v ~ Basis (DualSpace v) )
    => HasMetric v where
  type DualSpace v :: *
  type DualSpace v = v
  (<.>^) :: DualSpace v -> v -> Scalar v
  

(^<.>) :: HasMetric v => v -> DualSpace v -> Scalar v
ket ^<.> bra = bra <.>^ ket

instance HasMetric Double where
  (<.>^) = (<.>)
instance (HasMetric v, HasMetric w, Scalar v ~ Scalar w) => HasMetric (v,w) where
  type DualSpace (v,w) = (DualSpace v, DualSpace w)
  (v,w)<.>^(v',w') = v<.>^v' + w<.>^w'
instance HasMetric R2 where
  (<.>^) = (<.>)


-- | 'HerMetric' is a portmanteau of /Hermitian/ and /metric/ (in the sense as used
--   in e.g. general relativity).
newtype HerMetric v = HerMetric { getHerMetric :: DualSpace v :-* v }

instance HasMetric v => AdditiveGroup (HerMetric v) where
  zeroV = HerMetric zeroV
  negateV (HerMetric m) = HerMetric $ negateV m
  HerMetric m ^+^ HerMetric n = HerMetric $ m ^+^ n
instance HasMetric v => VectorSpace (HerMetric v) where
  type Scalar (HerMetric v) = Scalar v
  s *^ (HerMetric m) = HerMetric $ s *^ m 

projector :: HasMetric v => v -> HerMetric v
projector v = HerMetric (linear $ \u -> v ^* (v^<.>u))


metricSq, metric :: HasMetric v => HerMetric v -> DualSpace v -> Scalar v
metricSq (HerMetric m) v = v <.>^ lapply m v
metric (HerMetric m) v = sqrt $ v <.>^ lapply m v


metriScale :: HasMetric v => HerMetric v -> DualSpace v -> DualSpace v
metriScale m v = metric m v *^ v






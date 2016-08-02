-- |
-- Module      : Graphics.Dynamic.Plot.Internal.Types
-- Copyright   : (c) Justus Sagemüller 2015
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions


{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Graphics.Dynamic.Plot.Internal.Types where


import qualified Prelude

import Diagrams.Prelude ((^&), (&), _x, _y)
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.TwoD.Size as Dia
import qualified Diagrams.TwoD.Types as DiaTypes
import Diagrams.BoundingBox (BoundingBox)
import qualified Diagrams.BoundingBox as DiaBB
import qualified Diagrams.Backend.Cairo as Cairo
import qualified Diagrams.Backend.Cairo.Text as CairoTxt
    
import qualified Data.Colour as DCol

import qualified Control.Category.Hask as Hask
import Control.Category.Constrained.Prelude hiding ((^))
import Control.Arrow.Constrained
import Control.Monad.Constrained

import Control.Lens hiding ((...), (<.>))

import qualified Data.Vector as Arr
import Data.List (sort)

import Data.VectorSpace
import Data.Basis
import Data.AffineSpace
import Data.VectorSpace.Free ()
import Data.LinearMap.HerMetric
import Data.Manifold.PseudoAffine
import Data.Manifold.TreeCover
import Data.Semigroup
import Data.Tagged

import Control.DeepSeq

type R2 = Dia.V2 Double
type P2 = Dia.P2 Double

instance FiniteDimensional R2 where
  dimension = Tagged 2
  basisIndex = Tagged bi where bi b = if (basisValue b::R2)^._x > 0.5 then 0 else 1
  indexBasis = Tagged ib
   where ib 0 = bx; ib 1 = by
         [(bx,_), (by,_)] = decompose (1^&1 :: R2)
  completeBasis = Tagged . fmap fst $ decompose (1^&1 :: R2)
instance HasMetric' R2 where
  type DualSpace R2 = R2
  (<.>^) = (<.>)
  functional f = f(1^&0) ^& f(0^&1)
  doubleDual = id; doubleDual' = id
instance Semimanifold R2 where
  type Needle R2 = R2
  fromInterior = id
  toInterior = pure
  translateP = Tagged (^+^)
  (.+~^) = (^+^)
instance PseudoAffine R2 where
  p.-~.q = pure(p^-^q)
instance LocallyCoercible R2 (R,R) where
  locallyTrivialDiffeomorphism v = (v^._x, v^._y)
instance LocallyCoercible (R,R) R2 where
  locallyTrivialDiffeomorphism = DiaTypes.r2

instance Semimanifold P2 where
  type Needle P2 = R2
  fromInterior = id
  toInterior = pure
  translateP = Tagged (.+^)
  (.+~^) = (.+^)
instance PseudoAffine P2 where
  p.-~.q = pure(p.-.q)
instance LocallyCoercible P2 (R,R) where
  locallyTrivialDiffeomorphism v = (v^._x, v^._y)
instance LocallyCoercible (R,R) P2 where
  locallyTrivialDiffeomorphism = DiaTypes.p2



(^) :: Num n => n -> Int -> n
(^) = (Prelude.^)


type R = Double

-- | Use 'Graphics.Dynamic.Plot.R2.plot' to directly include any 'Dia.Diagram'.
--   (All 'Graphics.Dynamic.Plot.R2.DynamicPlottable'
--   is internally rendered to that type.)
-- 
--   The exact type may change in the future: we'll probably stay with @diagrams@,
--   but when document output is introduced the backend might become variable 
--   or something else but 'Cairo.Cairo'.
type PlainGraphicsR2 = Dia.Diagram Cairo.B





  

data Pair p = Pair !p !p
       deriving (Hask.Functor, Show, Eq, Ord)
data Triple p = Triple !p !p !p
       deriving (Hask.Functor, Show, Eq, Ord)

data DiffList a = DiffList { getDiffList :: [a]->[a], diffListLen :: Int }
diffList :: Arr.Vector a -> DiffList a
diffList l = DiffList (Arr.toList l++) (Arr.length l)

instance Semigroup (DiffList a) where
  DiffList dl n <> DiffList dl' n' = DiffList (dl . dl') (n+n')
instance Monoid (DiffList a) where
  mappend = (<>); mempty = DiffList id 0


newtype SplitList a = SplitList { getSplList :: Arr.Vector a }
       deriving (Hask.Functor, Monoid)
presplitList :: [a] -> SplitList a
presplitList = SplitList . Arr.fromList

splitEvenly :: Int -> SplitList a -> Either (Arr.Vector a) [SplitList a]
splitEvenly k _ | k < 1  = error "Can't split a list to less than one part."
splitEvenly k (SplitList v)
  | k >= n     = Left v
  | otherwise  = Right $ splits splitIs 0
 where splitIs = take k . map round . tail
                    $ iterate (+ (fromIntegral n/fromIntegral k :: Double)) 0
       splits [_] i₀ = [SplitList $ Arr.drop i₀ v]
       splits (i:is) i₀ = SplitList (Arr.slice i₀ (i-i₀) v) : splits is i
       n = Arr.length v

instance Semigroup (SplitList a) where
  SplitList l <> SplitList l' = SplitList (l Arr.++ l')

fromDiffList :: DiffList a -> SplitList a
fromDiffList (DiffList f _) = SplitList . Arr.fromList $ f[]




data LinFitParams y = LinFitParams { constCoeff :: y
                                   , linCoeff :: Diff y }
deriving instance (AffineSpace y, Show y, Show (Diff y)) => Show (LinFitParams y)


linFitMeanInCtrdUnitIntv ::
     (AffineSpace y, v~Diff y, VectorSpace v, Fractional (Scalar v))
                                 => LinFitParams y -> y
linFitMeanInCtrdUnitIntv (LinFitParams{..}) = constCoeff



data DevBoxes y = DevBoxes { deviations :: HerMetric' (Diff y)
                           , maxDeviation :: Scalar (Diff y)   }
                




data PCMRange x = PCMRange { pcmStart, pcmSampleDuration :: x } deriving (Show)
 
data RecursiveSamples' n x y t
   = RecursivePCM { rPCMlinFit :: LinFitParams y
                  , details :: Either (Pair (RecursiveSamples' n x y t))
                                      (Arr.Vector (y,t))
                  , pFitDeviations :: DevBoxes y
                  , samplingSpec :: PCMRange x
                  , splIdLen :: Int
                  , rPCMNodeInfo :: n
                  }
instance Hask.Functor (RecursiveSamples' n x y) where
  fmap f (RecursivePCM l d v s n i) = RecursivePCM l d' v s n i
   where d' = case d of Left rs' -> Left (fmap (fmap f) rs')
                        Right ps -> Right $ fmap (second f) ps

fmapRPCMNodeInfo :: (n->n') -> RecursivePCM n x y -> RecursivePCM n' x y
fmapRPCMNodeInfo f (RecursivePCM l d v s n i) = RecursivePCM l d' v s n $ f i
 where d' = case d of Left rs' -> Left (fmap (fmapRPCMNodeInfo f) rs')
                      Right ps -> Right ps

type RecursiveSamples = RecursiveSamples' ()
type RecursivePCM n x y = RecursiveSamples' n x y ()
type (x-.^>y) = RecursivePCM () x y

recursiveSamples' :: forall x y v t .
          ( VectorSpace x, Real (Scalar x)
          , AffineSpace y, v~Diff y, InnerSpace v, HasMetric v, RealFloat (Scalar v) )
                     => PCMRange x -> [(y,t)] -> RecursiveSamples x y t
recursiveSamples' xrng_g ys = calcDeviations . go xrng_g $ presplitList ys
    where go :: PCMRange x -> SplitList (y,t) -> RecursiveSamples' (Arr.Vector y) x y t
          go xrng@(PCMRange xl wsp) l@(SplitList arr) = case splitEvenly 2 l of
             Right sps
              | [sp1, sp2] <- lIndThru xl sps
                     -> let pFit = solveToLinFit
                               $ (linFitMeanInCtrdUnitIntv.rPCMlinFit) <$> [sp1,sp2]
                        in RecursivePCM pFit
                                        (Left $ Pair sp1 sp2)
                                        (undefined)
                                        xrng (Arr.length arr)
                                        (fmap fst arr)
             Right _ -> evenSplitErr
             Left pSpls -> RecursivePCM (solveToLinFit $ Arr.toList (fmap fst pSpls))
                                        (Right $ pSpls)
                                        (undefined)
                                        xrng (Arr.length arr)
                                        (fmap fst arr)
           where lIndThru _ [] = []
                 lIndThru x₀₁ (sp₁@(SplitList arr₁):sps)
                        = let x₀₂ = x₀₁ ^+^ fromIntegral (Arr.length arr₁) *^ wsp
                          in go (PCMRange x₀₁ wsp) sp₁ : lIndThru x₀₂ sps          
          evenSplitErr = error "'splitEvenly' returned wrong number of slices."
          
          calcDeviations :: RecursiveSamples' (Arr.Vector y) x y t
                         -> RecursiveSamples x y t
          calcDeviations = cdvs Nothing Nothing
           where cdvs lPFits rPFits
                         rPCM@( RecursivePCM pFit dtls _ sSpc@(PCMRange xl wsp) slLn pts )
                    = RecursivePCM pFit dtls' (DevBoxes stdDev maxDev) sSpc slLn ()
                   where stdDev = (^/ fromIntegral slLn) . sumV $ projector' <$> msqs
                         maxDev =     sqrt           . maximum $ magnitudeSq <$> msqs
                         msqs = [ (y .-. ff x)
                                | (x,y) <- normlsdIdd $ SplitList pts ]
                         ff = l₀splineRep (Pair lPFits rPFits) rPCM
                         dtls' = case dtls of
                             Left (Pair r₁ r₂)
                               -> let r₁' = cdvs (rRoute=<<lPFits) (Just r₂) r₁
                                      r₂' = cdvs (Just r₁) (lRoute=<<rPFits) r₂
                                  in Left $ Pair r₁' r₂'
                             Right pSpls -> Right pSpls
                         (LinFitParams b a) = pFit
lRoute, rRoute :: RecursiveSamples' n x y t -> Maybe (RecursiveSamples' n x y t)
lRoute (RecursivePCM {details = Right _}) = Nothing
lRoute (RecursivePCM {details = Left (Pair l _)}) = Just l
rRoute (RecursivePCM {details = Right _}) = Nothing
rRoute (RecursivePCM {details = Left (Pair _ r)}) = Just r
                         

recursiveSamples :: 
          ( AffineSpace y, v~Diff y, InnerSpace v, HasMetric v, RealFloat (Scalar v) )
                     => [(y,t)] -> RecursiveSamples Int y t
recursiveSamples = recursiveSamples' (PCMRange 0 1)

recursivePCM :: ( VectorSpace x, Real (Scalar x)
                , AffineSpace y, v~Diff y, InnerSpace v, HasMetric v, RealFloat (Scalar v) )
                     => PCMRange x -> [y] -> x-.^>y
recursivePCM xrng_g = recursiveSamples' xrng_g . fmap (,())


splineRep :: ( AffineSpace y, v~Diff y, InnerSpace v, Floating (Scalar v), Ord (Scalar v) )
                     => Int         -- ^ Number of subdivisions to \"go down\".
                        -> (R-.^>y) -> R -> y
splineRep n₀ rPCM@(RecursivePCM _ _ _ (PCMRange xl wsp) slLn ())
              = go n₀ Nothing Nothing rPCM . normaliseR
 where go n lPFits rPFits (RecursivePCM _ (Left (Pair r₁ r₂)) _ _ slLn ())
         | n>0, f₁ <- go (n-1) (rRoute=<<lPFits) (Just r₂) r₁
              , f₂ <- go (n-1) (Just r₁) (lRoute=<<rPFits) r₂
                =  \x -> if x<0.5 then f₁ $ x*2
                                  else f₂ $ x*2 - 1
       go _ lPFits rPFits rPCM = l₀splineRep (Pair lPFits rPFits) rPCM
       
       normaliseR x = (x - xl)/(wsp * fromIntegral slLn)

l₀splineRep ::
          ( VectorSpace x, Num (Scalar x)
          , AffineSpace y, v~Diff y, VectorSpace v, Floating (Scalar v), Ord (Scalar v) )
                     => Pair (Maybe (RecursiveSamples' n x y t'))
                           -> (RecursiveSamples' n x y t)
                            -> R{-Sample position normalised to [0,1]-} -> y
l₀splineRep (Pair lPFits rPFits)
            (RecursivePCM{ rPCMlinFit=LinFitParams b a
                         , samplingSpec=PCMRange x₀ wsp
                         , splIdLen = n })
               = f
 where f x | x < 0.5, t <- realToFrac $ 0.5 - x
           , Just(RecursivePCM{rPCMlinFit=LinFitParams b'l a'l}) <- lPFits
                        = b .+^ (b'l.-.b) ^* h₀₁ t
                            .-^ a ^* h₁₀ t
                            .-^ a'l ^* h₁₁ t
           | x > 0.5, t <- realToFrac $ x - 0.5
           , Just(RecursivePCM{rPCMlinFit=LinFitParams b'r a'r}) <- rPFits
                        = b .+^ (b'r.-.b) ^* h₀₁ t
                            .+^ a ^* h₁₀ t
                            .+^ a'r ^* h₁₁ t
           | t <- realToFrac $ x-0.5
                        = b .+^ t*^a
       h₀₀ t = (1 + 2*t) * (1 - t)^2  -- Cubic Hermite splines
       h₀₁ t = t^2 * (3 - 2*t)
       h₁₀ t = t * (1 - t)^2
       h₁₁ t = t^2 * (t - 1)



rPCMSample :: (AffineSpace y, v~Diff y, InnerSpace v, HasMetric v, RealFloat (Scalar v))
       => Interval R -> R -> (R->y) -> R-.^>y
rPCMSample (Interval l r) δx f = recursivePCM (PCMRange l δx) [f x | x<-[l, l+δx .. r]] 
                   

type R2Box = Dia.BoundingBox Dia.V2 Double

rPCM_R2_boundingBox :: (RecursiveSamples x P2 t) -> R2Box
rPCM_R2_boundingBox rPCM@(RecursivePCM pFit _ (DevBoxes dev _) _ _ ())
          =    Interval (xl - ux*2) (xr + ux*2)
           -*| Interval (yb - uy*2) (yt + uy*2)
 where pm = constCoeff pFit
       p₀ = pm .-^ linCoeff pFit; pe = pm .+^ linCoeff pFit
       ux = metric' dev $ 1^&0; uy = metric' dev $ 0^&1
       [xl,xr] = sort[p₀^._x, pe^._x]; [yb,yt] = sort[p₀^._y, pe^._y]





rPCMLinFitRange :: (R-.^>R) -> Interval R -> Interval R
rPCMLinFitRange rPCM@(RecursivePCM _ _ (DevBoxes _ δ) _ _ ()) ix
             = let (Interval b t) = rppm rPCM ix in Interval (b-δ) (t+δ)
 where rppm rPCM@(RecursivePCM (LinFitParams b a) _ _ _ _ ()) (Interval l r)
         | r < (-1)   = spInterval $ b - a
         | l > 1      = spInterval $ b + a
         | l < (-1)   = rppm rPCM $ Interval (-1) r
         | r > 1      = rppm rPCM $ Interval l 1
         | otherwise  = (b + l*a) ... (b + r*a)


solveToLinFit :: (AffineSpace y, v~Diff y, VectorSpace v, Floating (Scalar v))
                        => [y] -> LinFitParams y
solveToLinFit [] = error
        "LinFit solve under-specified (need at least one reference point)."
solveToLinFit [y] = LinFitParams { constCoeff=y, linCoeff=zeroV }
solveToLinFit [y₁,y₂]  -- @[x₁, x₂] ≡ [-½, ½]@, and @f(½) = (y₁+y₂)/2 + ½·(y₂-y₁) = y₂@.
                       -- (Likewise for @f(-½) = y₁@).
      = LinFitParams { constCoeff = alerp y₁ y₂ 0.5
                     , linCoeff = y₂ .-. y₁ }
solveToLinFit _ = error "LinFit solve over-specified (can't solve more than two points)."


normlsdIdd :: Fractional x => SplitList y -> [(x, y)]
normlsdIdd (SplitList l) = zip [ (k+1/2)/fromIntegral (Arr.length l)
                               | k<-iterate(+1)0] $ Arr.toList l


type FColour = DCol.Colour Double
type AColour = DCol.AlphaColour Double

-- | Unlike the typical types such as 'Draw.Color', this one has /semantic/ 
--   more than physical meaning.
data Colour = BaseColour BaseColour
            | Contrast BaseColour
            | Paler Colour
            | CustomColour FColour
            deriving (Eq)
data BaseColour = Neutral -- ^ Either black or white, depending on the context.
                | Red     -- ^ Contrast cyan.
                | Yellow  -- ^ Contrast violet.
                | Green   -- ^ Contrast magenta.
                | Blue    -- ^ Contrast orange.
                deriving (Eq, Show, Enum)

type ColourScheme = Colour -> AColour

data GraphWindowSpecR2 = GraphWindowSpecR2 {
      lBound, rBound, bBound, tBound :: R
    , xResolution, yResolution :: Int
    , colourScheme :: ColourScheme
  }
instance Show GraphWindowSpecR2 where
  show (GraphWindowSpecR2{..}) = "GraphWindowSpecR2{\
                               \lBound="++show lBound++", \
                               \rBound="++show rBound++", \
                               \bBound="++show bBound++", \
                               \tBound="++show tBound++", \
                               \xResolution="++show xResolution++", \
                               \yResolution="++show yResolution++"}"




data Interval r = Interval !r !r deriving (Show)
instance (Ord r) => Semigroup (Interval r) where  -- WRT closed hull of the union.
  Interval l₁ u₁ <> Interval l₂ u₂ = Interval (min l₁ l₂) (max u₁ u₂)

realInterval :: Real r => Interval r -> Interval R
realInterval (Interval a b) = Interval (realToFrac a) (realToFrac b)

onInterval :: ((R,R) -> (R,R)) -> Interval R -> Interval R
onInterval f (Interval l r) = uncurry Interval $ f (l, r)

infixl 6 ...
-- | Build an interval from specified boundary points. No matter which of these
--   points is higher, the result will always be the interval in between (i.e.,
--   @3 '...' 1@ will yield the interval [1,3], not an empty set or some \"oriented
--   interval\" [3,1]).
--   The fixity @infixl 6@ was chosen so you can write 2D bounding-boxes as e.g.
--   @-1...4 -*| -1...1@.
(...) :: (Ord r) => r -> r -> Interval r
x1...x2 | x1 < x2    = Interval x1 x2
        | otherwise  = Interval x2 x1

infixl ±
(±) :: Real v => v -> v -> Interval v
c ± δ | δ>0        = Interval (c-δ) (c+δ)
      | otherwise  = Interval (c+δ) (c-δ)

spInterval :: r -> Interval r
spInterval x = Interval x x

intersects :: Ord r => Interval r -> Interval r -> Bool
intersects (Interval a b) (Interval c d) = a<=d && b>=c

includes :: Ord r => Interval r -> r -> Bool
Interval a b `includes` x = x>=a && x<=b

infix 5 -*|

-- | Cartesian product of intervals.
(-*|) :: Interval R -> Interval R -> R2Box
Interval l r -*| Interval b t = DiaBB.fromCorners (l^&b) (r^&t)

-- | Inverse of @uncurry ('-*|')@. /This is a partial function/, since
--   'BoundingBox'es can be empty.
xyRanges :: R2Box -> (Interval R, Interval R)
xyRanges bb = let Just (c₁, c₂) = DiaBB.getCorners bb
              in (c₁^._x ... c₂^._x, c₁^._y ... c₂^._y)



shadeExtends :: Shade P2 -> (Interval R, Interval R)
shadeExtends shade
      = ( (ctr^._x) ± sqrt (metric' expa $ 1^&0)
        , (ctr^._y) ± sqrt (metric' expa $ 0^&1) )
 where ctr = shade^.shadeCtr; expa = shade^.shadeExpanse







type Necessity = Double
superfluent = -1e+32 :: Necessity






infixl 7 `provided`
provided :: Monoid m => m -> Bool -> m
provided m True = m
provided m False = mempty


ceil, flor :: R -> R
ceil = fromInt . ceiling
flor = fromInt . floor

fromInt :: Num a => Int -> a
fromInt = fromIntegral





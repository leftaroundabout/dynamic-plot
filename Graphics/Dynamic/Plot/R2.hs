-- |
-- Module      : Graphics.Dynamic.Plot.R2
-- Copyright   : (c) Justus Sagemüller 2013-2014
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions


{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Graphics.Dynamic.Plot.R2 (
        -- * Interactive display
          plotWindow
        -- * Plottable objects
        -- ** Class  
        , Plottable(..)
        -- ** Simple function plots 
        , fnPlot, paramPlot
        , continFnPlot
        , tracePlot
        -- ** View selection
        , xInterval, yInterval
        -- ** Plot type
        , DynamicPlottable
        ) where

import Graphics.Dynamic.Plot.Colour



import qualified Prelude

-- import Graphics.DrawingCombinators ((%%), R, R2)
-- import qualified Graphics.DrawingCombinators as Draw
-- import qualified Graphics.UI.GLFW as GLFW
-- import qualified Graphics.Rendering.OpenGL as OpenGL
-- import Graphics.Rendering.OpenGL (($=))
import Diagrams.Prelude (R2, P2, (^&), (&), _x, _y)
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.TwoD.Size as Dia
import qualified Diagrams.TwoD.Types as DiaTypes
import Diagrams.BoundingBox (BoundingBox)
import qualified Diagrams.BoundingBox as DiaBB
import qualified Diagrams.Backend.Cairo as Cairo
import qualified Diagrams.Backend.Cairo.Text as CairoTxt
    
import qualified Data.Colour as DCol

import qualified Diagrams.Backend.Gtk as BGTK
import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk ( AttrOp((:=)) )
import qualified Graphics.UI.Gtk.Gdk.EventM as Event
import qualified System.Glib.Signals (on)

import Control.Monad.Trans (liftIO)

import qualified Control.Category.Hask as Hask
import Control.Category.Constrained.Prelude hiding ((^))
import Control.Arrow.Constrained
import Control.Monad.Constrained

import Control.Lens hiding ((...), (<.>))

  
import Control.Concurrent.Async
import Control.DeepSeq


import Data.List (foldl', sort, intercalate, isPrefixOf, isInfixOf, find, zip4)
import qualified Data.Vector as Arr
import Data.Maybe
import Data.Semigroup
import Data.Foldable (fold, foldMap)
import Data.Function (on)

import Data.VectorSpace
import Data.AffineSpace
import Data.LinearMap.HerMetric
import qualified Data.Map.Lazy as Map

import Data.Manifold ((:-->))
import qualified Data.Manifold as 𝓒⁰
  
import Text.Printf

import Data.IORef

import System.IO
import System.Exit
import System.Process
import Data.Time




instance HasMetric R2 where
  type DualSpace R2 = R2
  (<.>^) = (<.>)

(^) :: Num n => n -> Int -> n
(^) = (Prelude.^)


type R = Double

type Diagram = Dia.Diagram Cairo.B R2




class Plottable p where
  plot :: p -> DynamicPlottable

instance (RealFloat r₁, RealFloat r₂) => Plottable (r₁ -> r₂) where
  plot f = continFnPlot $ realToFrac . f . realToFrac

-- {-# RULES "plot/R->R" plot = fnPlot #-}

instance Plottable (Double :--> Double) where
  plot f = DynamicPlottable{
             relevantRange_x = const mempty
           , relevantRange_y = fmap yRangef
           , isTintableMonochromic = True
           , axesNecessity = 1
           , dynamicPlot = plot }
   where yRangef (Interval l r) = uncurry Interval . (minimum &&& maximum) 
                            . map snd $ 𝓒⁰.finiteGraphContinℝtoℝ
                                         (𝓒⁰.GraphWindowSpec l r fgb fgt 9 9) f
          where (fgb, fgt) = (minimum &&& maximum) [f $ l, f $ m, f $ r]
                m = l + (r-l) * 0.352479608143
         
         plot (GraphWindowSpec{..}) = curve `deepseq` Plot [] (trace curve)
          where curve :: [Dia.P2]
                curve = map convℝ² $ 𝓒⁰.finiteGraphContinℝtoℝ mWindow f
                mWindow = 𝓒⁰.GraphWindowSpec (c lBound) (c rBound) (c bBound) (c tBound) 
                                                 xResolution yResolution
                trace (p:q:ps) = simpleLine p q <> trace (q:ps)
                trace _ = mempty
         
         convℝ² = Dia.p2
         c = realToFrac

instance Plottable (Double :--> (Double, Double)) where
  plot f = DynamicPlottable{
             relevantRange_x = const mempty
           , relevantRange_y = const mempty
           , isTintableMonochromic = True
           , axesNecessity = 1
           , dynamicPlot = plot }
   where plot (GraphWindowSpec{..}) = curves `deepseq` Plot [] (foldMap trace curves)
          where curves :: [[Dia.P2]]
                curves = map (map convℝ²) $ 𝓒⁰.finiteGraphContinℝtoℝ² mWindow f
                mWindow = 𝓒⁰.GraphWindowSpec (c lBound) (c rBound) (c bBound) (c tBound) 
                                                 xResolution yResolution
                trace (p:q:ps) = simpleLine p q <> trace (q:ps)
                trace _ = mempty
         
         convℝ² = Dia.p2
         c = realToFrac


instance (Plottable p) => Plottable [p] where
  plot l0 = DynamicPlottable{
              relevantRange_x = \ry -> foldMap (($ry) . relevantRange_x) l
            , relevantRange_y = \rx -> foldMap (($rx) . relevantRange_y) l
            , isTintableMonochromic = or $ isTintableMonochromic <$> l
            , axesNecessity = sum $ axesNecessity <$> l
            , dynamicPlot = foldMap dynamicPlot l
            }
   where l = map plot l0

instance Plottable Diagram where
  plot d = DynamicPlottable{
             relevantRange_x = const $ Option rlx
           , relevantRange_y = const $ Option rly
           , isTintableMonochromic = False
           , axesNecessity = -1
           , dynamicPlot = plot
           }
   where bb = DiaBB.boundingBox d
         (rlx,rly) = case DiaBB.getCorners bb of
                       Just (c1, c2)
                        -> ( Just $ c1^._x ... c2^._x
                           , Just $ c1^._y ... c2^._y )
         plot _ = Plot [] d



  

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



data DevBoxes y = DevBoxes { deviations :: HerMetric (Diff y)
                           , maxDeviation :: Scalar (Diff y) }
                




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
                   where stdDev = (^/ fromIntegral slLn) . sumV $ projector <$> msqs
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
                   

instance Plottable (R-.^>R) where
  plot rPCM@(RecursivePCM gPFit gDetails gFitDevs (PCMRange x₀ wsp) gSplN ())
            = DynamicPlottable{
                relevantRange_x = const . pure $ Interval x₀ xr
              , relevantRange_y = fmap $ rPCMLinFitRange rPCM
              , isTintableMonochromic = True
              , axesNecessity = 1
              , dynamicPlot = plot
              }
   where 
         xr = wsp * fromIntegral gSplN
         plot (GraphWindowSpec{..}) = Plot [] . trace $ flattenPCM_resoCut bb δx rPCM
          where 
                trace dPath = fold [ trMBound [ p & _y +~ s*δ
                                             | (p, DevBoxes _ δ) <- dPath ]
                                  | s <- [-1, 1] ]
                             <> trStRange dPath
                trStRange ((p,DevBoxes σp' δp) : qd@(q,DevBoxes σq' δq) : ps)
                     = (let η = (σp/δp + σq/δq)/2
                        in Dia.opacity (1-η)
                            (Dia.strokeLocLoop (Dia.fromVertices
                             [_y+~σq $ q, _y+~σp $ p, _y-~σp $ p, _y-~σq $ q
                             ,_y+~σq $ q ]))
                        <> Dia.opacity (η^2)
                            (Dia.strokeLocLoop (Dia.fromVertices
                             [_y+~δq $ q, _y+~δp $ p, _y-~δp $ p, _y-~δq $ q
                             ,_y+~δq $ q ]))
                       ) <> trStRange (qd:ps)
                 where [σp,σq] = map (`metric`1) [σp', σq']
                trStRange _ = mempty
                trMBound l = Dia.fromVertices l & Dia.dashingO [2,2] 0
                
                w = rBound - lBound; h = tBound - bBound
                δx = w * 3/fromIntegral xResolution
                bb = Interval lBound rBound
                 -*| Interval (bBound - h) (tBound + h) -- Heuristic \"buffering\",
                      -- to account for the missing ability of 'flattenPCM_resoCut' to
                      -- take deviations from quadratic-fit into account.
  

instance Plottable (RecursiveSamples Int P2 (DevBoxes P2)) where
  plot rPCM@(RecursivePCM gPFit gDetails gFitDevs (PCMRange t₀ τsp) gSplN ())
            = DynamicPlottable{
                relevantRange_x = const $ pure xRange
              , relevantRange_y = const $ pure yRange
              , isTintableMonochromic = True
              , axesNecessity = 1
              , dynamicPlot = plot
              }
   where plot (GraphWindowSpec{..}) = Plot []
                        . foldMap trStRange
                        $ flattenPCM_P2_resoCut bbView [(1/δxl)^&0, 0^&(1/δyl)] rPCM
          where trStRange (Left appr) = trSR $ map calcNormDev appr
                 where trSR ((pl,pr) : qd@(ql,qr) : ps)
                        = Dia.opacity 0.3
                               (Dia.strokeLocLoop (Dia.fromVertices
                                [ ql, pl, pr, qr, ql ]
                          )) <> trSR (qd:ps)
                       trSR _ = mempty
                       calcNormDev ((p,v), DevBoxes σ _) = (p .+^ d, p .-^ d)
                        where d = metriScale σ $ turnLeft v
                trStRange (Right pts) = (`foldMap`pts)
                   $ \(p, DevBoxes dv _)
                              -> let δxm = metric dv $ 1^&0
                                     δym = metric dv $ 0^&1
                                 in if δxm > δx && δym > δy
                                      then simpleLine (_x +~ δxm $ p) (_x -~ δxm $ p)
                                            <> simpleLine (_y +~ δym $ p) (_y -~ δym $ p)
                                      else (Dia.rect (max δx $ δxm*2) (max δy $ δym*2)
                                                & Dia.moveTo p)
                
                w = rBound - lBound; h = tBound - bBound
                δxl = 6 * δx; δyl = 6 * δy
                δx = w/fromIntegral xResolution; δy = h/fromIntegral yResolution
                bbView = Interval lBound rBound -*| Interval bBound tBound
         bb = rPCM_R2_boundingBox rPCM
         (xRange,yRange) = xyRanges bb



instance Plottable (Int -.^> P2) where
  plot = plot . fmap (\() -> DevBoxes zeroV zeroV :: DevBoxes P2)



-- | Plot a sequence of points @(x,y)@. The appearance of the plot will be automatically
--   chosen to match resolution and point density: at low densities, each point will simply
--   get displayed on its own. When the density goes so high you couldn't distinguish
--   individual points anyway, we switch to a &#x201c;trace view&#x201d;, approximating
--   the probability density function around a &#x201c;local mean path&#x201d;, which is
--   rather more insightful (and much less obstructive/clunky) than a simple cloud of
--   independent points.
--   
--   In principle, this should be able to handle vast amounts of data
--   (so you can, say, directly plot an audio file); at the moment the implementation
--   isn't efficient enough and will get slow for more than some 100000 data points.
tracePlot :: [(Double, Double)] -> DynamicPlottable
tracePlot = plot . recursiveSamples . map ((,()) . Dia.p2)
  

flattenPCM_resoCut :: BoundingBox R2 -> R -> (R-.^>R) -> [(P2, DevBoxes R)]
flattenPCM_resoCut bb δx = case DiaBB.getCorners bb of
                             Nothing -> const []
                             Just cs -> ($[]) . go' cs
 where go' cs@(lCorn,rCorn) = go where
        go rPCM@(RecursivePCM pFit details fitDevs (PCMRange x₁ wsp) splN ())
          | DiaBB.isEmptyBox $ DiaBB.intersection bb sqRange
                = id
          | w > δx, Left (Pair s1 s2) <- details
                = go s1 . go s2
          | otherwise 
                = ((xm ^& constCoeff pFit, fitDevs) :)
         where xr = x₁ + w
               xm = x₁ + w / 2
               w = wsp * fromIntegral splN
               sqRange = xRange -*| rPCMLinFitRange rPCM xRange_norm'd
               xRange = x₁ ... xr
               xRange_norm'd = max (-1) ((lCorn^._x - xm)/w)
                           ... min   1  ((rCorn^._x - xm)/w)

flattenPCM_P2_resoCut :: BoundingBox R2 -> [DualSpace R2]
                              -> (RecursiveSamples x P2 t)
                              -> [ Either [((P2, R2), DevBoxes P2)]
                                          [(P2, t)]                 ]
flattenPCM_P2_resoCut bb δs = case DiaBB.getCorners bb of
                                Nothing -> const []
                                Just cs -> ($[]) . go' cs
 where go' cs@(lCorn,rCorn) = go where
        go rPCM@(RecursivePCM (LinFitParams pm pa) details fitDevs@(DevBoxes dev _) _ _ ())
          | DiaBB.isEmptyBox $ DiaBB.intersection bb (rPCM_R2_boundingBox rPCM)
                = \case l@(Left [] : _) -> l
                        l -> Left [] : l
          | metrics dev δs > 0.5 || (sum $ ((^2).(pa<.>^)) <$> δs) > 3
          , Left (Pair s1 s2) <- details
                = go s1 . go s2
          | Right pts <- details = (Right (Arr.toList pts) :)
          | otherwise 
                = \case
                     (Left h : r) -> Left (((pm, dir), fitDevs) : h) : r
                     r -> Left [((pm, dir), fitDevs)] : r
         where dir = case magnitude pa of 0 -> zeroV; m -> pa ^/ m

turnLeft :: R2 -> R2
turnLeft (DiaTypes.R2 x y) = DiaTypes.R2 (-y) x


rPCM_R2_boundingBox :: (RecursiveSamples x P2 t) -> BoundingBox R2
rPCM_R2_boundingBox rPCM@(RecursivePCM pFit _ (DevBoxes dev _) _ _ ())
          =    Interval (xl - ux*2) (xr + ux*2)
           -*| Interval (yb - uy*2) (yt + uy*2)
 where pm = constCoeff pFit
       p₀ = pm .-^ linCoeff pFit; pe = pm .+^ linCoeff pFit
       ux = metric dev $ 1^&0; uy = metric dev $ 0^&1
       [xl,xr] = sort[p₀^._x, pe^._x]; [yb,yt] = sort[p₀^._y, pe^._y]



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


rPCMLinFitRange :: (R-.^>R) -> Interval R -> Interval R
rPCMLinFitRange rPCM@(RecursivePCM _ _ (DevBoxes _ δ) _ _ ()) ix
             = let (Interval b t) = rppm rPCM ix in Interval (b-δ) (t+δ)
 where rppm rPCM@(RecursivePCM (LinFitParams b a) _ _ _ _ ()) (Interval l r)
         | r < (-1)   = spInterval $ b - a
         | l > 1      = spInterval $ b + a
         | l < (-1)   = rppm rPCM $ Interval (-1) r
         | r > 1      = rppm rPCM $ Interval l 1
         | otherwise  = (b + l*a) ... (b + r*a)



rPCMPlot :: [R] -> DynamicPlottable
rPCMPlot = plot . recursivePCM (PCMRange (0 :: Double) 1)

-- plotSamples :: [R2]





data GraphWindowSpec = GraphWindowSpec {
      lBound, rBound, bBound, tBound :: R
    , xResolution, yResolution :: Int
    , colourScheme :: ColourScheme
  }
instance Show GraphWindowSpec where
  show (GraphWindowSpec{..}) = "GraphWindowSpec{\
                               \lBound="++show lBound++", \
                               \rBound="++show rBound++", \
                               \bBound="++show bBound++", \
                               \tBound="++show tBound++", \
                               \xResolution="++show xResolution++", \
                               \yResolution="++show yResolution++"}"

moveStepRel :: (R, R)  -- ^ Relative translation @(Δx/w, Δy/h)@.
            -> (R, R)  -- ^ Relative zoom.
            -> GraphWindowSpec -> GraphWindowSpec
moveStepRel (δx,δy) (ζx,ζy) (GraphWindowSpec l r b t xRes yRes clSchm)
  = GraphWindowSpec l' r' b' t' xRes yRes clSchm
 where qx = (r-l)/2                  ; qy = (t-b)/2
       mx'= l + qx*(1+δx)            ; my'= b + qy*(1+δy) 
       qx'= zoomSafeGuard mx' $ qx/ζx; qy'= zoomSafeGuard my' $ qy/ζy
       l' = mx' - qx'                ; b' = my' - qy'
       r' = mx' + qx'                ; t' = my' + qy'
       zoomSafeGuard m = max (1e-250 + abs m*1e-6) . min 1e+250



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

spInterval :: r -> Interval r
spInterval x = Interval x x

intersects :: Ord r => Interval r -> Interval r -> Bool
intersects (Interval a b) (Interval c d) = a<=d && b>=c

includes :: Ord r => Interval r -> r -> Bool
Interval a b `includes` x = x>=a && x<=b

infix 5 -*|

-- | Cartesian product of intervals.
(-*|) :: Interval R -> Interval R -> BoundingBox R2
Interval l r -*| Interval b t = DiaBB.fromCorners (l^&b) (r^&t)

-- | Inverse of @uncurry ('-*|')@. /This is a partial function/, since
--   'BoundingBox'es can be empty.
xyRanges :: BoundingBox R2 -> (Interval R, Interval R)
xyRanges bb = let Just (c₁, c₂) = DiaBB.getCorners bb
              in (c₁^._x ... c₂^._x, c₁^._y ... c₂^._y)








data Plot = Plot {
       plotAnnotations :: [Annotation]
     , getPlot :: Diagram
  }
instance Semigroup Plot where
  Plot a1 d1 <> Plot a2 d2 = Plot (a1<>a2) (d1<>d2)
instance Monoid Plot where
  mempty = Plot mempty mempty
  mappend = (<>)

data DynamicPlottable = DynamicPlottable { 
        relevantRange_x, relevantRange_y :: Option (Interval R) -> Option (Interval R)
      , isTintableMonochromic :: Bool
      , axesNecessity :: Necessity
      , dynamicPlot :: GraphWindowSpec -> Plot
  }

data GraphViewState = GraphViewState {
        lastStableView :: Maybe (GraphWindowSpec, Plot)
      , realtimeView, nextTgtView :: Async Plot
      , graphColor :: Maybe AColour
   }




                

-- | Plot some plot objects to a new interactive GTK window. Useful for a quick
--   preview of some unknown data or real-valued functions; things like selection
--   of reasonable view range and colourisation are automatically chosen.
--   
--   Example:
-- 
-- @
--     plotWindow [ fnPlot cos
--                , tracePlot [(x,y) | x<-[-1,-0.96..1]
--                                   , y<-[0,0.01..1]
--                                   , abs (x^2 + y^2 - 1) < 0.01 ]]
-- @
-- 
--   This gives such a plot window:
-- 
--   <<images/examples/cos-encircle-points.png>>
-- 
--   And that can with the mouse wheel be zoomed/browsed, like
-- 
--   <<images/examples/cos-encircle-points-far.png>>
--  
--   The individual objects you want to plot can be evaluated in multiple threads, so
--   a single hard calculatation won't freeze the responsitivity of the whole window.
--   Invoke e.g. from @ghci +RTS -N4@ to benefit from this.
plotWindow :: [DynamicPlottable] -> IO GraphWindowSpec
plotWindow [] = plotWindow [dynamicAxes]
plotWindow graphs' = do
   
   dgStore <- newIORef $ mempty
   
   
   let defColourScheme = defaultColourScheme
   
   
   ([viewTgt, viewState], graphs) <- do
           let window₀ = autoDefaultView graphs'
               assignGrViews :: [DynamicPlottable] -> [Colour] -> Double
                               -> IO [(DynamicPlottable, GraphViewState)]
               assignGrViews (g@DynamicPlottable{..}:gs) (c:cs) axn = do 
                   v <- async $ return $! dynamicPlot window₀
                   fmap ((g, GraphViewState { lastStableView = Nothing
                                            , realtimeView = v, nextTgtView = v 
                                            , graphColor = cl }
                        ) : ) $ assignGrViews gs cs' (axn + axesNecessity)
                where (cl, cs')
                        | isTintableMonochromic  = (Just $ defColourScheme c, cs)
                        | otherwise              = (Nothing, c:cs)
               assignGrViews [] _ axesNeed 
                 | axesNeed > 0  = assignGrViews [dynamicAxes] [grey] (-1)
                 | otherwise     = return []
           w <- mapM newIORef $ replicate 2 window₀
           gs <- newIORef =<< assignGrViews graphs' defaultColourSeq 0
           return (w,gs)
   
   
   GTK.initGUI
   window <- GTK.windowNew
                 
   refreshDraw <- do
       drawA <- GTK.drawingAreaNew
       GTK.onExpose drawA $ \_ -> do
                (canvasX,canvasY) <- GTK.widgetGetSize drawA
                modifyIORef viewTgt $ \view -> view{ xResolution = fromIntegral canvasX
                                                   , yResolution = fromIntegral canvasY }
                dia <- readIORef dgStore
                let oldSize = Dia.size2D dia
                    scaledDia = Dia.bg Dia.black
                                . Dia.scaleX (fromInt canvasX / 2)
                                . Dia.scaleY (-fromInt canvasY / 2)
                                . Dia.translate (1 ^& (-1))
                                . Dia.withEnvelope (Dia.rect 2 2 :: Diagram)
                                  $ dia
                drawWindow <- GTK.widgetGetDrawWindow drawA
                -- putStrLn $ "redrawing"++show(canvasX,canvasY)
                -- putStrLn . ("with state now:\n"++) . show =<< readIORef viewState
                BGTK.renderToGtk drawWindow $ scaledDia
                -- putStrLn $ "redrawn."
                return True
       
       GTK.on drawA GTK.scrollEvent . Event.tryEvent $ do
                (canvasX,canvasY) <- liftIO $ GTK.widgetGetSize drawA
                (scrollX,scrollY) <- Event.eventCoordinates
                let (rcX,rcY) = ( scrollX*2 / fromIntegral canvasX - 1
                                , 1 - scrollY*2 / fromIntegral canvasY )
                scrollD <- Event.eventScrollDirection
                case defaultScrollBehaviour scrollD of
                   ScrollZoomIn  -> liftIO $ do
                     modifyIORef viewTgt $ \view@GraphWindowSpec{..}
                         -> let w = rBound - lBound
                                h = tBound - bBound
                            in view{ lBound = lBound + w * (rcX + 1)^2 * scrollZoomStrength
                                   , rBound = rBound - w * (rcX - 1)^2 * scrollZoomStrength
                                   , tBound = tBound - h * (rcY - 1)^2 * scrollZoomStrength
                                   , bBound = bBound + h * (rcY + 1)^2 * scrollZoomStrength
                                   }
                   ScrollZoomOut -> liftIO $ do
                     modifyIORef viewTgt $ \view@GraphWindowSpec{..}
                         -> let w = rBound - lBound
                                h = tBound - bBound
                            in view{ lBound = lBound - w * (rcX - 1)^2 * scrollZoomStrength
                                   , rBound = rBound + w * (rcX + 1)^2 * scrollZoomStrength
                                   , tBound = tBound + h * (rcY + 1)^2 * scrollZoomStrength
                                   , bBound = bBound - h * (rcY - 1)^2 * scrollZoomStrength
                                   }
                       
                       
       
       GTK.set window [ GTK.windowTitle := "Plot"
                      , GTK.windowDefaultWidth := defResX
                      , GTK.windowDefaultHeight := defResY
                      , GTK.containerChild := drawA
                      ]
       
       GTK.widgetShowAll window
       
       return $ GTK.widgetQueueDraw drawA
       
   
   let updateRTView, updateTgtView :: (GraphWindowSpec -> GraphWindowSpec) -> IO ()
       updateRTView updRealView = do
          vstOld <- readIORef viewState
          let newRealView = updRealView vstOld
          grViewsOld <- readIORef graphs
          writeIORef graphs <=< forM grViewsOld $ 
               \(o@DynamicPlottable{..}, gv) -> do
                  newRt <- async $ return $! dynamicPlot newRealView
                  poll (realtimeView gv) >>= \case
                    Just(Right vw) -> return (o
                      , gv{ realtimeView = newRt, lastStableView = Just (vstOld, vw) })
                    _ -> do 
                       cancel $ realtimeView gv
                       poll (nextTgtView gv) >>= \case
                         Just(Right vw) -> do
                           ttvn <- readIORef viewTgt 
                           return (o, gv{ realtimeView = newRt, lastStableView = Just (ttvn, vw) })
                         _ -> return (o, gv{ realtimeView = newRt })
          writeIORef viewState newRealView
       updateTgtView updTgtView = do
          newTgtView <- updTgtView <$> readIORef viewTgt
          grViewsOld <- readIORef graphs
          writeIORef graphs <=< forM grViewsOld $ 
               \(o@DynamicPlottable{..}, gv) -> do
                  newTt <- async $ return $! dynamicPlot newTgtView
                  cancel $ nextTgtView gv
                  return (o, gv{ nextTgtView = newTt })
          writeIORef viewTgt newTgtView
   
   t₀ <- getCurrentTime
   lastFrameTime <- newIORef t₀
   
   let minKeyImpact = 0.05
   
   keyImpactState <- newIORef $ Map.fromList [ (ka, (t₀, minKeyImpact)) | ka<-[MoveLeft .. ZoomOut_y] ]
   
   
   let refreshScreen = do
           currentView@(GraphWindowSpec{..}) <- readIORef viewState
           let normaliseView :: Diagram -> Diagram
               normaliseView = (Dia.scaleX xUnZ :: Diagram->Diagram) . Dia.scaleY yUnZ
                                . Dia.translate (Dia.r2(-x₀,-y₀))
                  where xUnZ = 1/w; yUnZ = 1/h
               w = (rBound - lBound)/2; h = (tBound - bBound)/2
               x₀ = lBound + w; y₀ = bBound + h
               renderComp (DynamicPlottable{..}, GraphViewState{..}) = do
                   plt <- poll realtimeView >>= \case
                                  Just (Right pl) -> return $ Just pl
                                  _ -> case lastStableView of
                                   Just (_, vw) -> return $ Just vw
                                   _ -> poll nextTgtView >> return Nothing
                   return $ case plt of
                    Nothing -> mempty
                    Just Plot{..} -> let 
                       antTK = DiagramTK { viewScope = currentView 
                                         , textTools = TextTK defaultTxtStyle
                                                                  txtSize aspect 0.2 0.2 }
                       txtSize = h * fontPts / fromIntegral yResolution
                       aspect  = w * fromIntegral yResolution
                                                         / (h * fromIntegral xResolution)
                       fontPts = 12
                       transform :: Diagram -> Diagram
                       transform = normaliseView . clr
                         where clr | Just c <- graphColor  = Dia.lcA c . Dia.fcA c
                                   | otherwise             = id
                     in transform $ foldMap (prerenderAnnotation antTK) plotAnnotations
                                 <> getPlot

           gvStates <- readIORef graphs
           waitAny $ map (realtimeView . snd) gvStates
                   
           writeIORef dgStore
                . mconcat . reverse =<< mapM renderComp (reverse gvStates)
                                                    
           refreshDraw
           
   let mainLoop = do
           t <- getCurrentTime
           δt <- fmap (diffUTCTime t) $ readIORef lastFrameTime
           writeIORef lastFrameTime t
   
           do vt <- readIORef viewTgt
              updateRTView $ \vo -> 
                   let a%b = let η = min 1 $ 2 * realToFrac δt in η*a + (1-η)*b 
                   in GraphWindowSpec (lBound vt % lBound vo) (rBound vt % rBound vo)
                                      (bBound vt % bBound vo) (tBound vt % tBound vo)
                                      (xResolution vt) (yResolution vt)
                                      defColourScheme
           -- GTK.sleep 0.01
           refreshScreen
           -- GTK.pollEvents
           return True
   
   let keyImpact key = do
           t <- getCurrentTime
           Just (_, impact) <- fmap (Map.lookup key) $ readIORef keyImpactState
           modifyIORef keyImpactState $ Map.adjust ( \(t₁, p)
                       -> (t, min 1 $ ( (p - minKeyImpact) * (exp . (*3) . realToFrac $ diffUTCTime t₁ t)
                                       + minKeyImpact ) * 2 )
                   ) key
           return impact
   
--    GLFW.keyCallback $= \key state -> do
--            let keyStepSize = 0.1
--            (state==GLFW.Press) `when` do
--               case defaultKeyMap key of
--                 Just QuitProgram -> writeIORef done True
--                 Just movement    -> do
--                    impact <- keyImpact movement
--                    updateTgtView $ case movement of
--                     MoveUp    -> moveStepRel (0,  impact) (1, 1)
--                     MoveDown  -> moveStepRel (0, -impact) (1, 1)
--                     MoveLeft  -> moveStepRel (-impact, 0) (1, 1)
--                     MoveRight -> moveStepRel (impact , 0) (1, 1)
--                     ZoomIn_x  -> moveStepRel (0, 0)   (1+impact, 1)
--                     ZoomOut_x -> moveStepRel (0, 0)   (1-impact/2, 1)
--                     ZoomIn_y  -> moveStepRel (0, 0)   (1, 1+impact/2)
--                     ZoomOut_y -> moveStepRel (0, 0)   (1, 1-impact/2)
--                 _ -> return ()
--            
   GTK.onDestroy window $ do
        (readIORef graphs >>=) . mapM_  -- cancel remaining threads
           $ \(_, GraphViewState{..}) -> cancel realtimeView >> cancel nextTgtView
        GTK.mainQuit
                 
   
   -- putStrLn "Enter Main loop..."
   
--    mainLoop
   GTK.timeoutAdd mainLoop 100
   

   GTK.mainGUI
   
   -- putStrLn "Done."
   
   -- GTK.mainQuit
   
   readIORef viewState


autoDefaultView :: [DynamicPlottable] -> GraphWindowSpec
autoDefaultView graphs = GraphWindowSpec l r b t defResX defResY defaultColourScheme
  where (xRange, yRange) = foldMap (relevantRange_x &&& relevantRange_y) graphs
        ((l,r), (b,t)) = ( xRange `dependentOn` yRange
                         , yRange `dependentOn` xRange )
        ξ`dependentOn`υ = addMargin . defRng . ξ . return . defRng $ υ mempty
        defRng = Interval (-1) 1 `option` id
        addMargin (Interval a b) = (a - q, b + q)
            where q = (b - a) / 6
  


-- render :: Diagram -> IO()
-- render = Dia.clearRender

defResX, defResY :: Integral i => i
defResX = 640
defResY = 480


data ScrollAction = ScrollZoomIn | ScrollZoomOut

defaultScrollBehaviour :: Event.ScrollDirection -> ScrollAction
defaultScrollBehaviour Event.ScrollUp = ScrollZoomIn
defaultScrollBehaviour Event.ScrollDown = ScrollZoomOut

scrollZoomStrength :: Double
scrollZoomStrength = 1/20


data KeyAction = MoveLeft
               | MoveRight
               | MoveUp
               | MoveDown
               | ZoomIn_x
               | ZoomOut_x
               | ZoomIn_y
               | ZoomOut_y
               | QuitProgram
   deriving (Eq, Ord, Enum)

defaultKeyMap :: GTK.KeyVal -> Maybe KeyAction
-- defaultKeyMap (GLFW.SpecialKey GLFW.UP   ) = Just MoveUp
-- defaultKeyMap (GLFW.SpecialKey GLFW.DOWN ) = Just MoveDown
-- defaultKeyMap (GLFW.SpecialKey GLFW.LEFT ) = Just MoveLeft
-- defaultKeyMap (GLFW.SpecialKey GLFW.RIGHT) = Just MoveRight
-- defaultKeyMap (GLFW.CharKey 'K') = Just MoveUp
-- defaultKeyMap (GLFW.CharKey 'J') = Just MoveDown
-- defaultKeyMap (GLFW.CharKey 'H') = Just MoveLeft
-- defaultKeyMap (GLFW.CharKey 'L') = Just MoveRight
-- defaultKeyMap (GLFW.CharKey 'B') = Just ZoomIn_x
-- defaultKeyMap (GLFW.CharKey 'N') = Just ZoomOut_x
-- defaultKeyMap (GLFW.CharKey 'I') = Just ZoomIn_y
-- defaultKeyMap (GLFW.CharKey 'O') = Just ZoomOut_y
-- defaultKeyMap (GLFW.SpecialKey GLFW.ESC) = Just QuitProgram
defaultKeyMap _ = Nothing

-- instance NFData Draw.R


-- | Plot an (assumed continuous) function in the usual way.
--   Since this uses functions of actual 'Double' values, you have more liberty
--   of defining functions with range-pattern-matching etc., which is at the moment
--   not possible in the ':-->' category.
-- 
--   However, because 'Double' can't really proove properties of a mathematical
--   function, aliasing and similar problems are not taken into account. So it only works
--   accurately when the function is locally linear on pixel scales (what most
--   other plot programs just assume silently). In case of singularities, the
--   naïve thing is done (extend as far as possible; vertical line at sign change),
--   which again is common enough though not really right.
--   
--   We'd like to recommend using 'fnPlot' whenever possible, which automatically adjusts
--   the resolution so the plot is guaranteed accurate (but it's not usable yet for
--   a lot of real applications).
continFnPlot :: (Double -> Double) -> DynamicPlottable
continFnPlot f = DynamicPlottable{
               relevantRange_x = const mempty
             , relevantRange_y = yRangef
             , isTintableMonochromic = True
             , axesNecessity = 1
             , dynamicPlot = plot }
 where yRangef = fmap . onInterval $ \(l, r) -> ((!10) &&& (!70)) . sort . pruneOutlyers
                                               $ map f [l, l + (r-l)/80 .. r]
       plot (GraphWindowSpec{..}) = curve `deepseq` Plot [] (trace curve)
        where δx = (rBound - lBound) * 2 / fromIntegral xResolution
              curve = [ (x ^& f x) | x<-[lBound, lBound+δx .. rBound] ]
              trace (p:q:ps) = simpleLine p q <> trace (q:ps)
              trace _ = mempty
       pruneOutlyers = filter (not . isNaN) 
       l!n | (x:_)<-drop n l  = x
           | otherwise         = error "Function appears to yield NaN most of the time. Cannot be plotted."

                                 
-- | Plot a continuous function in the usual way, taking arguments from the
--   x-Coordinate and results to the y one.
--   The signature looks more complicated than it is; think about it as requiring
--   a polymorphic 'Floating' function. Any simple expression like
--   @'fnPlot' (\\x -> sin x / exp (x^2))@ will work (but the view must not contain
--   singularities).
--   
--   Under the hood this uses the category of continuous functions, ':-->', to proove
--   that no details are omitted (like small high-frequency bumps). The flip side is that
--   this does not always work very efficiently, in fact it can easily become exponentially
--   slow for some parameters.
--   Make sure to run multithreaded, to prevent hanging your program this way. Also consider
--   limiting the memory: if you try to plot across singularities, the program may well
--   eat up all available resorces before failing. (But it will never &#x201c;succeed&#x201d; and
--   plot something wrong!)
--   
--   In the future, we would like to switch to the category of piecewise continuously-differentiable
--   functions. That wouldn't suffer from said problems, and should
--   also generally be more efficient. (That category is not yet implemented in Haskell.)
fnPlot :: (forall m . 𝓒⁰.Manifold m 
                   => ProxyVal (:-->) m Double -> ProxyVal (:-->) m Double) 
                      -> DynamicPlottable
fnPlot f = plot fc
 where fc :: Double :--> Double
       fc = alg f
       
-- | Plot a continuous, &#x201c;parametric function&#x201d;, i.e. mapping the real
--   line to a path in &#x211d;&#xb2;.
paramPlot :: (forall m . 𝓒⁰.Manifold m 
                    => ProxyVal (:-->) m Double 
                        -> (ProxyVal (:-->) m Double, ProxyVal (:-->) m Double)) 
                     -> DynamicPlottable
paramPlot f = plot fc
 where fc :: Double :--> (Double, Double)
       fc = alg1to2 f




data AxesStyle = DynamicAxesStyle
data DynamicAxes = DynamicAxes { yAxisClasses, xAxisClasses :: [AxisClass] }
data AxisClass = AxisClass { visibleAxes :: [Axis], axisStrength :: Double, decPrecision :: Int }
data Axis = Axis { axisPosition :: R }

crtDynamicAxes :: GraphWindowSpec -> DynamicAxes
crtDynamicAxes (GraphWindowSpec {..}) = DynamicAxes yAxCls xAxCls
 where [yAxCls, xAxCls] = zipWith3 directional 
                        [lBound, bBound] [rBound, tBound] [xResolution, yResolution]
       directional l u res = map lvl lvlSpecs
        where span = u - l
              upDecaSpan = 10**(ceil $ lg span)
              pixelScale = span / (fromIntegral res * upDecaSpan)
              baseDecaval = upDecaSpan * (flor $ l / upDecaSpan)
              lvl (minSpc, strength) 
                = AxisClass [ Axis v  | i<-[0 .. luDSdiv*2]
                                      , let v=(baseDecaval + i*laSpc), v>l, v<u ] 
                            strength
                            (floor $ lg laSpc)
               where laSpc = upDecaSpan / luDSdiv
                     luDSdiv = ll -- maybe 1 id . listToMaybe 
                                . takeWhile (\d -> pixelScale * minSpc < 1/d )
                                      . join $ iterate (map(*10)) [1, 2, 5]
                     ll [] = error $ "pixelScale = "++show pixelScale
                                   ++"; minSpc = "++show minSpc
                     ll l = last l
       lvlSpecs = [ (80, 0.3), (18, 0.1) ]



dynamicAxes :: DynamicPlottable
dynamicAxes = DynamicPlottable { 
               relevantRange_x = const mempty
             , relevantRange_y = const mempty   
             , isTintableMonochromic = False
             , axesNecessity = superfluent
             , dynamicPlot = plot }
 where plot gwSpec@(GraphWindowSpec{..}) = Plot labels lines
        where (DynamicAxes yAxCls xAxCls) = crtDynamicAxes gwSpec
              lines = zeroLine (lBound^&0) (rBound^&0)  `provided`(bBound<0 && tBound>0)
                   <> zeroLine (0^&bBound) (0^&tBound)  `provided`(lBound<0 && rBound>0)
                   <> foldMap (renderClass $ \x -> (x^&bBound, x^&tBound)) yAxCls
                   <> foldMap (renderClass $ \y -> (lBound^&y, rBound^&y)) xAxCls
              labels = do (dirq, hAlign, vAlign, acl) <- zip4 [\x -> x^&0, \y -> 0^&y ] 
                                                              [AlignMid  , AlignTop   ]
                                                              [AlignTop  , AlignMid   ]
                                                              [yAxCls    , xAxCls     ]
                          let (AxisClass vaxs _ prc) = head acl
                              prepAnnotation (Axis{axisPosition=z}) = do
                                               guard(z/=0) 
                                               [Annotation (TextAnnotation txt align) place False]
                               where txt = PlainText . prettyFloatShow prc $ realToFrac z
                                     place = ExactPlace $ dirq z
                                     align = TextAlignment hAlign vAlign
                          prepAnnotation =<< vaxs
       zeroLine p1 p2 = simpleLine p1 p2 & Dia.lc Dia.grey
       renderClass crd (AxisClass axes strength _)
          = foldMap (uncurry simpleLine . crd . axisPosition) axes
             & Dia.lcA (Dia.grey `DCol.withOpacity` strength)



type Necessity = Double
superfluent = -1e+32 :: Necessity



simpleLine :: Dia.P2 -> Dia.P2 -> Diagram
simpleLine p q = Dia.fromVertices [p,q] & Dia.lwO 2



-- | When you &#x201c;plot&#x201d; 'xInterval' / 'yInterval', it is ensured that the (initial) view encompasses 
-- (at least) the specified range.
-- Note there is nothing special about these &#x201c;flag&#x201d; objects: /any/ 'Plottable' can request a 
-- certain view, e.g. for a discrete point cloud it's obvious and a function defines at least
-- a @y@-range for a given @x@-range. Only use explicit range when necessary.
xInterval, yInterval :: (Double, Double) -> DynamicPlottable
xInterval (l,r) = DynamicPlottable { 
               relevantRange_x = const . return $ Interval l r
             , relevantRange_y = const mempty
             , isTintableMonochromic = False
             , axesNecessity = 0
             , dynamicPlot = plot }
 where plot _ = Plot mempty mempty
yInterval (b,t) = DynamicPlottable { 
               relevantRange_x = const mempty
             , relevantRange_y = const . return $ Interval b t
             , isTintableMonochromic = False
             , axesNecessity = 0
             , dynamicPlot = plot }
 where plot _ = Plot mempty mempty
 

prettyFloatShow :: Int -> Double -> String
prettyFloatShow _ 0 = "0"
prettyFloatShow preci x
    | preci >= 0, preci < 4  = show $ round x
    | preci < 0, preci > -2  = printf "%.1f" x
    | otherwise   = case ceiling (0.01 + lg (abs x/10^^(preci+1))) + preci of
                        0    | preci < 0  -> printf ("%."++show(-preci)++"f") x
                        expn | expn>preci -> printf ("%."++show(expn-preci)++"f*10^%i")
                                                      (x/10^^expn)                 expn
                             | otherwise  -> printf ("%i*10^%i")
                                                      (round $ x/10^^expn :: Int)  expn
                                      




maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

data Annotation = Annotation {
         getAnnotation :: AnnotationObj 
       , placement     :: AnnotationPlace
       , isOptional    :: Bool
   }
data AnnotationObj = TextAnnotation TextObj TextAlignment
data AnnotationPlace = ExactPlace R2

data TextObj = PlainText String
data TextAlignment = TextAlignment { hAlign, vAlign :: Alignment } -- , blockSpread :: Bool }
data Alignment = AlignBottom | AlignMid | AlignTop

data DiagramTK = DiagramTK { textTools :: TextTK, viewScope :: GraphWindowSpec }
data TextTK = TextTK { txtCairoStyle :: Dia.Style R2 -- Draw.Font
                     , txtSize, xAspect, padding, extraTopPad :: R }

defaultTxtStyle :: Dia.Style R2
defaultTxtStyle = mempty & Dia.fontSizeO 9
                         & Dia.fc Dia.grey
                         & Dia.lc Dia.grey


prerenderAnnotation :: DiagramTK -> Annotation -> Diagram
prerenderAnnotation (DiagramTK{ textTools = TextTK{..}, viewScope = GraphWindowSpec{..} }) 
                    (Annotation{..})
       | TextAnnotation (PlainText str) (TextAlignment{..}) <- getAnnotation
       , ExactPlace p₀ <- placement
            = let rnTextLines = map (CairoTxt.textVisualBounded txtCairoStyle) $ lines str
                  lineWidths = map ((/4 {- Magic number ??? -})
                                . Dia.width) rnTextLines
                  nLines = length lineWidths
                  lineHeight = 1 + extraTopPad + 2*padding
                  ζx = ζy * xAspect
                  ζy = txtSize -- / lineHeight
                  width  = (maximum $ 0 : lineWidths) + 2*padding
                  height = fromIntegral nLines * lineHeight
                  y₀ = case vAlign of
                              AlignBottom -> padding + height - lineHeight
                              AlignMid    -> height/2 - lineHeight
                              AlignTop    -> - (lineHeight + padding)
                  fullText = mconcat $ zipWith3 ( \n w -> 
                                 let y = n*lineHeight
                                 in (Dia.translate $ Dia.r2 (case hAlign of 
                                      AlignBottom -> (padding       , y₀-y)
                                      AlignMid    -> (- w/2         , y₀-y)
                                      AlignTop    -> (-(w + padding), y₀-y)
                                     ) ) ) [0..] lineWidths rnTextLines
                  p = px ^& py
                   where px = max l' . min r' $ p₀^._x
                         py = max b' . min t' $ p₀^._y
                         (l', r') = case hAlign of
                           AlignBottom -> (lBound      , rBound - w  )
                           AlignMid    -> (lBound + w/2, rBound - w/2)
                           AlignTop    -> (lBound + w  , rBound      )
                         (b', t') = case vAlign of
                           AlignBottom -> (bBound      , tBound - h  )
                           AlignMid    -> (bBound + h/2, tBound - h/2)
                           AlignTop    -> (bBound + h  , tBound      )
                         w = ζx * width; h = ζy * height
              in Dia.translate p . Dia.scaleX ζx . Dia.scaleY ζy 
                     $ Dia.lc Dia.grey fullText
        





infixl 7 `provided`
provided :: Monoid m => m -> Bool -> m
provided m True = m
provided m False = mempty


lg :: Floating a => a -> a
lg x = log x / log 10


-- instance (Monoid v) => Semigroup (Draw.Image v) where
--   (<>) = mappend
-- instance Semigroup (Draw.Affine) where
--   (<>) = mappend
-- 
ceil, flor :: R -> R
ceil = fromInt . ceiling
flor = fromInt . floor

fromInt :: Num a => Int -> a
fromInt = fromIntegral



instance NFData Dia.P2


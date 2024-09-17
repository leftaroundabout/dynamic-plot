-- |
-- Module      : Graphics.Dynamic.Plot.R2.Internal
-- Copyright   : (c) Justus Sagemüller 2013-2019
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : requires GHC>6 extensions


{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LiberalTypeSynonyms        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE CPP                        #-}

module Graphics.Dynamic.Plot.R2.Internal where

import Graphics.Dynamic.Plot.Colour
import Graphics.Dynamic.Plot.Internal.Types
import Graphics.Text.Annotation

import qualified Prelude

import Diagrams.Prelude ((^&), (&), _x, _y)
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.TwoD.Size as Dia
import qualified Diagrams.TwoD.Types as DiaTypes
import Diagrams.TwoD.Types (V2(V2))
import Diagrams.BoundingBox (BoundingBox)
import qualified Diagrams.BoundingBox as DiaBB
import qualified Diagrams.Backend.Cairo as Cairo
import qualified Diagrams.Backend.Cairo.Text as CairoTxt
    
import qualified Data.Colour as DCol
import qualified Data.Colour.SRGB as DCol (toSRGB24, RGB(..))
import qualified Data.Colour.Names as DCol
import qualified Codec.Picture as JPix
import qualified Codec.Picture.Types as JPix

import Graphics.Image.Resample (refiningScaleX2Bilinear)

import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.State (evalState, get, put)
import Control.Monad.ST
import Control.Applicative ((<|>))
import Data.STRef

import qualified Control.Category.Hask as Hask
import Control.Category.Constrained.Prelude hiding ((^))
import Control.Arrow.Constrained
import Control.Monad.Constrained

import Control.Lens hiding ((...), (<.>))
import Control.Lens.TH(makeLenses)

  
import Control.Concurrent (runInBoundThread, threadDelay, ThreadId, forkIO, killThread)
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception (evaluate)


import Data.List (foldl', sort, sortBy, partition, zip4)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Vector as Arr
import Data.Maybe
import Data.Semigroup
import Data.Default
import Data.Foldable (fold, foldMap, minimumBy)
import qualified Data.Foldable as Hask
import Data.Function (on)
import Data.Ord (comparing)

import Data.VectorSpace
import Math.LinearMap.Category
import Data.Basis
import Data.AffineSpace
import Data.Manifold.PseudoAffine
#if MIN_VERSION_manifolds(0,6,0)
import Data.Manifold.WithBoundary
#endif
import Data.Function.Differentiable
import Data.Manifold.Types
import Data.Manifold.Shade
import Data.Manifold.TreeCover
import Data.Manifold.Web
import Data.Manifold.Riemannian (Geodesic, pointsBarycenter)
import qualified Data.Map.Lazy as Map

import qualified Data.Colour.Manifold as CSp
import qualified Data.Colour.Manifold.Internal as CSp

import qualified Data.Random as Random
import qualified System.Random as Random
import qualified Data.Random.Manifold
#if MIN_VERSION_random_fu(0,3,0)
import qualified System.Random.Stateful as Random (StateGenM, runStateGen)
import Data.RVar (pureRVar)
#endif

import Data.IORef

import Data.Time



data Plot = Plot {
       _plotAnnotations :: [Annotation]
     , _getPlot :: PlainGraphicsR2
  }
makeLenses ''Plot


data RangeRequest r 
       = OtherDimDependantRange (Maybe (Interval r) -> Maybe (Interval r))
       | MustBeThisRange (Interval r)

type GraphWindowSpec = GraphWindowSpecR2

type AxisLabel = (ℝ², String)

data Interactions x = Interactions {
        _mouseClicksCompleted :: [MouseEvent x]
      , _currentDragEndpoints :: Maybe (MouseEvent x)
      }
 deriving (Eq)
instance Semigroup (Interactions x) where
  Interactions cca cda<>Interactions ccb cdb = Interactions (cca<>ccb) (cda<|>cdb)
instance Monoid (Interactions x) where
  mempty = Interactions [] Nothing
  mappend = (<>)

data DynamicPlottable' m = DynamicPlottable { 
        _relevantRange_x, _relevantRange_y :: RangeRequest R
      , _viewportConstraint :: GraphWindowSpec -> GraphWindowSpec
      , _inherentColours :: [PColour]
      , _occlusiveness :: Double
         -- ^ How surface-occupying the plot is.
         --   Use positive values for opaque 2D plots that would tend to obscure
         --   other objects, negative values for sparse/small point plots.
         --   The z-order will be chosen accordingly.
      , _axesNecessity :: Necessity
      , _frameDelay :: NominalDiffTime
      , _legendEntries :: [LegendEntry]
      , _axisLabelRequests :: [AxisLabel]
      , _futurePlots :: Interactions (ℝ,ℝ) -> Maybe (DynamicPlottable' m)
      , _dynamicPlotWithAxisLabels :: [AxisLabel] -> GraphWindowSpec -> m Plot
  }
makeLenses ''DynamicPlottable'

dynamicPlot :: Setter' (DynamicPlottable' m) (GraphWindowSpec -> m Plot)
dynamicPlot = dynamicPlotWithAxisLabels . mapped

sustained :: Hask.Functor m
         => Setter' (DynamicPlottable' m) a -> Setter' (DynamicPlottable' m) a
sustained q = sets $ \f p -> p & q %~ f
                               & futurePlots %~ fmap (fmap $ sustained q %~ f)

allDynamicPlot :: Hask.Functor m => Setter' (DynamicPlottable' m)
                                            (GraphWindowSpec -> m Plot)
allDynamicPlot = sustained dynamicPlot


type DynamicPlottable = DynamicPlottable' Random.RVar

type AnnotPlot = (Plot, ([LegendEntry],[AxisLabel]))

data ObjInPlot = ObjInPlot {
        _lastStableView :: IORef (Maybe (GraphWindowSpec, AnnotPlot))
      , _newPlotView :: MVar (GraphWindowSpec, AnnotPlot)
      , _mouseEventsForObj :: MVar (Interactions (ℝ,ℝ))
      , _plotObjColour :: Maybe AColour
      , _originalPlotObject :: DynamicPlottable
   }
makeLenses ''ObjInPlot


newtype PlainGraphics = PlainGraphics { getPlainGraphics :: PlainGraphicsR2 }
    deriving (Semigroup, Monoid)




-- | Class for types that can be plotted in some canonical, &#x201c;obvious&#x201d;
--   way. If you want to display something and don't know about any specific caveats,
--   try just using 'plot'!
class Plottable p where
  plot :: p -> DynamicPlottable

instance Plottable DynamicPlottable where
  plot = id

instance Plottable (R -> R) where
  plot f = continFnPlot $ realToFrac . f . realToFrac

-- {-# RULES "plot/R->R" plot = fnPlot #-}



instance (Plottable p) => Plottable [p] where
  plot = foldMap plot
instance (Plottable p) => Plottable (Maybe p) where
  plot = foldMap plot

instance Plottable PlainGraphics where
  plot (PlainGraphics d) = case DiaBB.getCorners bb of
     Just (c1, c2) -> let (rlx,rly) = ( c1^._x ... c2^._x
                                      , c1^._y ... c2^._y ) in def
           & relevantRange_x .~ atLeastInterval rlx
           & relevantRange_y .~ atLeastInterval rly
           & inherentColours .~ [TrueColour DCol.grey]
           & axesNecessity .~ -1
           & dynamicPlot .~ pure.plot
     Nothing -> mempty
   where bb = DiaBB.boundingBox d
         plot _ = mkPlot d


-- | Use a generic diagram within a plot.
-- 
--   Like with the various specialised function plotters, this will get automatically
--   tinted to be distinguishable from other plot objects in the same window.
--   Use 'diagramPlot' instead, if you want to view the diagram as-is.
shapePlot :: PlainGraphicsR2 -> DynamicPlottable
shapePlot d = diagramPlot d
               & inherentColours .~ []
               & axesNecessity .~ 0

-- | Plot a generic 'Dia.Diagram'.
diagramPlot :: PlainGraphicsR2 -> DynamicPlottable
diagramPlot d = plot $ PlainGraphics d


metricFromLength :: ∀ s . RealFrac' s => s -> Norm s 
metricFromLength l | l>0   = case closedScalarWitness :: ClosedScalarWitness s of
       ClosedScalarWitness -> spanNorm [1 / l]
  
instance Plottable (R-->R) where
  plot f = def & relevantRange_y .~ OtherDimDependantRange yRangef
               & autoTint
               & axesNecessity .~ 1
               & dynamicPlot .~ pure.plot
   where yRangef Nothing = Nothing
         yRangef (Just (Interval l r))
             = case intervalImages
                      100
                      ( const . metricFromLength $ (r-l)/16 , const $ metricFromLength 0.0001 )
                      ( alg (\x -> ( point l?<x?<point r ?-> (f$~x) ))) of 
                 ([],[]) -> Nothing
                 (liv,riv) -> pure . foldr1 (<>) . map (uncurry Interval . snd)
                               $ take 4 liv ++ take 4 riv
         
         plot gs@(GraphWindowSpecR2{..}) = curves `deepseq`
                                          mkPlot (foldMap trace curves)
          where curves :: [[P2]]
                curves = map (map $ convℝ² . snd) . gatherSides
                        $ discretisePathSegs
                              10000
                              ( const . metricFromLength
                                               $ (rBound-lBound)/fromIntegral xResolution
                              , coerceMetric $ resolutionFunction gs )
                              ((id&&&f)
                               . alg (\x -> ( point lBound?<x?<point rBound ?-> x )))
                trace (p:q:ps) = simpleLine p q <> trace (q:ps)
                trace _ = mempty
                gatherSides = uncurry (++) . (take 50 *** take 50)
         
         convℝ² = Dia.p2
         c = realToFrac

instance Plottable (R-->(R,R)) where
  plot f = def & relevantRange_y .~ mempty
               & autoTint
               & axesNecessity .~ 1
               & dynamicPlot .~ pure.plot
   where plot gs@(GraphWindowSpecR2{..}) = curves `deepseq`
                                          mkPlot (foldMap trace curves)
          where curves :: [[P2]]
                curves = map (map $ convℝ² . snd) . gatherSides
                        $ discretisePathSegs
                              1000
                              ( const . metricFromLength $ 1/100
                              , coerceMetric $ resolutionFunction gs )
                              f
                trace (p:q:ps) = simpleLine p q <> trace (q:ps)
                trace _ = mempty
                gatherSides = uncurry (++) . (take 50 *** take 50)
         
         convℝ² = Dia.p2
         c = realToFrac


resolutionFunction :: GraphWindowSpecR2 -> RieMetric ℝ²
resolutionFunction GraphWindowSpecR2{..} = resoFunc
 where w = rBound - lBound; h = tBound - bBound
       ε = spanNorm [(recip δx^&0), (0^&recip δy)]
       δx = w / fromIntegral xResolution
       δy = h / fromIntegral yResolution
       resoFunc (DiaTypes.V2 x y)
         | x >= lBound, x <= rBound, y >= bBound, y <= tBound  = ε
         | otherwise = spanNorm [(recip qx^&0), (0^&recip qy)]
        where qx | x < lBound  = lBound - x
                 | x > rBound  = x - rBound
                 | otherwise   = δx * qy/δy
              qy | y < bBound  = bBound - y
                 | y > tBound  = y - tBound
                 | otherwise   = δy * qx/δx


instance Plottable (R-.^>R) where
  plot rPCM@(RecursivePCM gPFit gDetails gFitDevs (PCMRange x₀ wsp) gSplN ())
            = def
              & relevantRange_x .~ atLeastInterval (Interval x₀ xr)
              & relevantRange_y .~ otherDimDependence (rPCMLinFitRange rPCM)
              & autoTint
              & axesNecessity .~ 1
              & dynamicPlot .~ plot
   where 
         xr = wsp * fromIntegral gSplN
         plot (GraphWindowSpecR2{..}) = pure . mkPlot . trace
                                          $ flattenPCM_resoCut bb δx rPCM
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
                 where [σp,σq] = map (|$|1) [σp', σq']
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
            = def
              & relevantRange_x .~ atLeastInterval xRange
              & relevantRange_y .~ atLeastInterval yRange
              & autoTint
              & axesNecessity .~ 1
              & dynamicPlot .~ plot
   where plot (GraphWindowSpecR2{..}) = pure . mkPlot
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
                        where d = let v' = turnLeft v in v' ^* (σ|$|v')
                trStRange (Right pts) = (`foldMap`pts)
                   $ \(p, DevBoxes dv _)
                              -> let δxm = dv |$| 1^&0
                                     δym = dv |$| 0^&1
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
  plot = plot . fmap (\() -> DevBoxes mempty zeroV :: DevBoxes P2)



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

-- | Simply connect the points by straight line segments, in the given order.
--   Beware that this will always slow down the performance when the list is large;
--   there is no &#201c;statistic optimisation&#201d; as in 'tracePlot'.
lineSegPlot :: [(Double, Double)] -> DynamicPlottable
lineSegPlot ps'
    | null ps        = mempty & autoTint
    | otherwise      = def
             & relevantRange_x .~ atLeastInterval'
                           ( foldMap (pure . spInterval . fst) (concat ps) )
             & relevantRange_y .~ atLeastInterval'
                           ( foldMap (pure . spInterval . snd) (concat ps) )
             & autoTint
             & axesNecessity .~ 1
             & dynamicPlot .~ pure . plot
 where plot (GraphWindowSpecR2{..}) = mkPlot (foldMap trace ps)
        where trace (p:q:ps) = simpleLine (Dia.p2 p) (Dia.p2 q) <> trace (q:ps)
              trace _ = mempty
       ps = filter ((>1) . length) $ safeSeg ps'
       safeSeg [] = [[]]
       safeSeg ((x,y):l) | x==x && not (isInfinite x) && y==y && not (isInfinite y)
                           = case safeSeg l of { h:r -> ((x,y):h):r }
                         | otherwise  = [] : safeSeg l


  

flattenPCM_resoCut :: R2Box -> R -> (R-.^>R) -> [(P2, DevBoxes R)]
flattenPCM_resoCut bb δx = case DiaBB.getCorners bb of
                             Nothing -> const []
                             Just cs -> ($ []) . go' cs
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

flattenPCM_P2_resoCut :: R2Box -> [DualVector R2]
                              -> (RecursiveSamples x P2 t)
                              -> [ Either [((P2, R2), DevBoxes P2)]
                                          [(P2, t)]                 ]
flattenPCM_P2_resoCut bb δs = case DiaBB.getCorners bb of
                                Nothing -> const []
                                Just cs -> ($ []) . go' cs
 where go' cs@(lCorn,rCorn) = go where
        go rPCM@(RecursivePCM (LinFitParams pm pa) details fitDevs@(DevBoxes dev _) _ _ ())
          | DiaBB.isEmptyBox $ DiaBB.intersection bb (rPCM_R2_boundingBox rPCM)
                = \case l@(Left [] : _) -> l
                        l -> Left [] : l
          | sum (normSq dev<$>δs) > 1/4 || (sum $ ((^2).(pa<.>^)) <$> δs) > 3
          , Left (Pair s1 s2) <- details
                = go s1 . go s2
          | Right pts <- details = (Right (Arr.toList pts) :)
          | otherwise 
                = \case
                     (Left h : r) -> Left (((pm, dir), fitDevs) : h) : r
                     r -> Left [((pm, dir), fitDevs)] : r
         where dir = case magnitude pa of 0 -> zeroV; m -> pa ^/ m

turnLeft :: R2 -> R2
turnLeft (DiaTypes.V2 x y) = DiaTypes.V2 (-y) x





rPCMPlot :: [R] -> DynamicPlottable
rPCMPlot = plot . recursivePCM (PCMRange (0 :: Double) 1)


instance Plottable (Shade P2) where
  plot shade = def
              & relevantRange_x .~ atLeastInterval xRange
              & relevantRange_y .~ atLeastInterval yRange
              & autoTint
              & axesNecessity .~ 1
              & dynamicPlot .~ plot
   where plot _ = pure . mkPlot $ foldMap axLine eigVs 
          where axLine eigV = simpleLine (ctr .-~^ eigV) (ctr .+~^ eigV)
         (xRange,yRange) = shadeExtends shade
         ctr = shade^.shadeCtr
         eigVs = normSpanningSystem $ shade^.shadeExpanse

instance Plottable (Shade ℝ²) where
  plot (Shade v e) = plot (Shade (Dia.P v) e :: Shade P2)

instance Plottable (Shade (R,R)) where
  plot sh = plot (coerceShade sh :: Shade R2)

instance Plottable (Shade' (R,R)) where
  plot shade = def
              & autoTint
              & axesNecessity .~ 1
              & dynamicPlot .~ plot
   where plot wSpec = pure . mkPlot $ Dia.circle 1
                            & Dia.scaleX w₁ & Dia.scaleY w₂
                            & Dia.rotate ϑ
                            & Dia.opacity 0.2
                            & Dia.moveTo ctr
          where [w₁,w₂] = recip . sqrt
                        . max (recip $ 100 * max ((wSpec^.windowDiameter)^2) ctrDistance)
                        . fst <$> [ev₁, ev₂]
                ctrDistance = distanceSq (shade^.shadeCtr) (wSpec^.windowCenter)
         ctr = Dia.p2 $ shade^.shadeCtr
         Norm expanr = shade^.shadeNarrowness
         [ev₁@(_,(e₁x,e₁y)),ev₂] = case eigen $ arr expanr of
                  (e₁:e₂:_) -> [e₁,e₂]
                  [e@(_,(vx,vy))] -> [e, (0,(-vx,vy))]
                  [] -> [(0,(1,0)), (0,(0,1))]
         ϑ = atan2 e₁y e₁x  Dia.@@ Dia.rad

instance Plottable (ConvexSet ℝ²) where
  plot EmptyConvex = mempty
  plot (ConvexSet hull intersects)
    = plot (ConvexSet (coerceShade hull) (coerceShade<$>intersects) :: ConvexSet (ℝ,ℝ))
instance Plottable (ConvexSet (R,R)) where
  plot EmptyConvex = mempty
  plot (ConvexSet hull intersects)
      = plot [ plot intersects
                 & tweakPrerendered (Dia.opacity
                                     (1 / fromIntegral (length intersects)) )
             , plot hull
                 & tweakPrerendered ( Dia.lwO 3
                              >>> Dia.opacity 1
                              >>> Dia.fcA (Dia.withOpacity Dia.grey 0.01) ) ]
         
instance Plottable (Shade' ℝ²) where
  plot sh = plot (coerceShade sh :: Shade' (ℝ,ℝ))
         
instance Plottable (Shade' P2) where
  plot (Shade' (Dia.P v) e) = plot (Shade' v e :: Shade' ℝ²)


instance Plottable (Shaded ℝ ℝ) where
  plot tr | length trivs' >= 2
          = def & relevantRange_x .~ atLeastInterval (Interval xmin xmax)
                & relevantRange_y .~ atLeastInterval (Interval ymin ymax)
                & autoTint
                & axesNecessity .~ 1
                & dynamicPlot .~ plot
   where plot grWS@(GraphWindowSpecR2{..}) = pure . mkPlot $
                            foldMap parallelogram trivs
                         <> (foldMap (singlePointFor grWS) leafPoints
                               -- & Dia.dashingO [2,3] 0
                               & Dia.opacity 0.4 )
          where parallelogram ((x,δx), ((y,δy), j))
                    = lLoop [ (x+δx)^&(y+δy+jδx), (x-δx)^&(y+δy-jδx)
                            , (x-δx)^&(y-δy-jδx), (x+δx)^&(y-δy+jδx) ]
                         & Dia.strokeLocLoop
                         & Dia.opacity 0.3
                 where jδx = j $ δx
         trivs' = sortBy (comparing fst) $ stiAsIntervalMapping tr
         trivs = NE.fromList $ ccδs trivs'
          where ccδs [(x, yq), (x', yq')] = [((x,δx),yq), ((x',δx),yq')]
                 where δx = (x' - x)/2
                ccδs [(x, yq), (x', yq'), (x'', yq'')]
                         = [((x,δx),yq), ((x',δx),yq'), ((x'',δx),yq'')]
                 where δx = (x'' - x)/4
                ccδs ((x, yq) : xyqs@((x', yq') : (x'', _) : _))
                         = ((x,δx),yq) : ((x',δx),yq') : tail (ccδs xyqs)
                 where δx = (x'' - x)/4
         [xmin, ymin, xmax, ymax]
            = [minimum, maximum]<*>[fst<$>allLeaves, snd<$>allLeaves]
         lLoop ps@(p:_) = Dia.fromVertices $ ps++[p]
         leafPoints = sortBy (comparing (^._x))
                         $ (\(x,y) -> y^&x) <$> allLeaves
         allLeaves = onlyLeaves tr
  plot _ = def

instance Plottable (PointsWeb ℝ (Shade' ℝ)) where
  plot web | length locals >= 2
          = def & relevantRange_x .~ atLeastInterval (Interval xmin xmax)
                & relevantRange_y .~ atLeastInterval (Interval ymin ymax)
                & autoTint
                & axesNecessity .~ 1
                & dynamicPlot .~ pure . plot
   where plot grWS@(GraphWindowSpecR2{..}) = mkPlot $
                            foldMap parallelogram trivs
                         <> foldMap vbar divis
          where parallelogram ((x,(δxl,δxr)), ((y,δy), j))
                    = lLoop [ (x+δxr)^&(y+δy+jδxr), (x-δxl)^&(y+δy-jδxl)
                            , (x-δxl)^&(y-δy-jδxl), (x+δxr)^&(y-δy+jδxr) ]
                         & Dia.strokeLocLoop
                         & Dia.opacity 0.3
                 where jδxl = j $ δxl
                       jδxr = j $ δxr
                vbar (x,(δxl,δxr)) = Dia.fromVertices
                            [ (x-δxl)^&tBound, (x-δxl)^&bBound
                            , (x+δxr)^&bBound, (x+δxr)^&tBound ]
         
         trivs :: [((ℝ, (Diff ℝ,Diff ℝ)), ((ℝ, Diff ℝ), LocalLinear ℝ ℝ))]
         divis :: [(ℝ, (Diff ℝ,Diff ℝ))]
         (trivs,divis) = concat***concat $ unzip (map mkTriv locals)
          where mkTriv :: ((ℝ, Shade' ℝ), [(ℝ, Shade' ℝ)])
                     -> ( [((ℝ, (Diff ℝ,Diff ℝ)), ((ℝ, Diff ℝ), LocalLinear ℝ ℝ))]
                        , [(ℝ, (Diff ℝ,Diff ℝ))] )
                mkTriv ((xc,Shade' yc yce), [(δxo, Shade' yo _)])
                       = case findNormalLength yce of
                           Just ry ->
                              ( [ ( (xc, dirSort 0 δxo)
                                  , ( (yc, ry)
                                    , id ^* ((yo-yc)/δxo) ) ) ], [] )
                           Nothing ->
                              ( [], [(xc, dirSort 0 δxo)] )
                mkTriv ((xc,Shade' yc yce), [(δxl, Shade' yl _), (δxr, Shade' yr _)])
                       = case findNormalLength yce of
                           Just ry ->
                              ( [ ( (xc, dirSort δxl δxr)
                                  , ( (yc, ry)
                                    , id ^* η ) ) ], [] )
                           Nothing ->
                              ( [], [(xc, dirSort δxl δxr)] )
                 where δxg = (δxr - δxl)/2
                       η = (yr - yl)/(2*δxg)
                mkTriv (p,lrs) = concat***concat $ unzip [mkTriv (p,[l,r]) | l<-ls, r<-rs]
                 where (ls,rs) = partition ((<0) . fst) lrs
                
                dirSort δ₁ δ₂ | δ₁ < δ₂    = (-δ₁, δ₂)
                              | otherwise  = (-δ₂, δ₁)
         
         lLoop ps@(p:_) = Dia.fromVertices $ ps++[p]
         
         [xmin, ymin, xmax, ymax]
            = [minimum, maximum]<*>[fst.fst<$>locals, (^.shadeCtr).snd.fst<$>locals]
         
         locals :: [((ℝ, Shade' ℝ), [(ℝ, Shade' ℝ)])]
         locals = Hask.toList $ localFocusWeb web
  plot _ = def

instance Plottable (PointsWeb ℝ² (CSp.Colour ℝ)) where
  plot web = plot (coerceWebDomain web :: PointsWeb (ℝ,ℝ) (CSp.Colour ℝ))

instance Plottable (PointsWeb (ℝ,ℝ) (CSp.Colour ℝ)) where
  plot = webbedSurfPlot $ pure . toRGBA
   where toRGBA (Just c)
             = JPix.promotePixel (CSp.quantiseColour c :: JPix.PixelRGB8)
         toRGBA _ = JPix.PixelRGBA8 0 0 0 0

#if MIN_VERSION_manifolds(0,6,0)
instance Plottable (PointsWeb ℝ² (Shade CSp.ColourNeedle)) where
#else
instance Plottable (PointsWeb ℝ² (Shade (CSp.Colour ℝ))) where
#endif
  plot web = plot (coerceWebDomain web :: PointsWeb (ℝ,ℝ) (Shade CSp.ColourNeedle))

#if MIN_VERSION_manifolds(0,6,0)
instance Plottable (PointsWeb (ℝ,ℝ) (Shade CSp.ColourNeedle)) where
#else
instance Plottable (PointsWeb (ℝ,ℝ) (Shade (CSp.Colour ℝ))) where
#endif
  plot = webbedSurfPlot toRGBA
   where toRGBA (Just c)
             = JPix.promotePixel . (CSp.quantiseColour :: CSp.Colour ℝ -> JPix.PixelRGB8)
#if MIN_VERSION_manifolds(0,6,0)
                . fromInterior
#endif
                                       <$> Random.rvar c
         toRGBA _ = return $ JPix.PixelRGBA8 0 0 0 0

instance Plottable (Cutplane (ℝ,ℝ)) where
  plot (Cutplane (x₀,y₀) (Stiefel1 (dy,dx)))
    = plot (Cutplane (V2 x₀ y₀) (Stiefel1 (V2 dy dx)))

instance Plottable (Cutplane ℝ²) where
  plot (Cutplane (V2 x₀ y₀) (Stiefel1 (V2 dy dx)))
          = def & autoTint
                & axesNecessity .~ 1
                & dynamicPlot .~ pure . plot
   where plot grWS@(GraphWindowSpecR2{..}) = mkPlot $ simpleLine p q
          where [p,q]
                 | abs (dy*(rBound-lBound)) > abs (dx*(tBound-bBound))
                    = [xf bBound ^& bBound, xf tBound ^& tBound]
                 | otherwise
                    = [lBound ^& yf lBound, rBound ^& yf rBound]
         yf x = y₀ - dy/dx * (x-x₀)
         xf y = x₀ - dx/dy * (y-y₀)


webbedSurfPlot :: Geodesic a
       => (Maybe a -> Random.RVar JPix.PixelRGBA8)
           -> PointsWeb (ℝ,ℝ) a -> DynamicPlottable
webbedSurfPlot toRGBA web = def & dynamicPlot .~ plotWeb
                                & relevantRange_x .~ atLeastInterval (x₀...x₁)
                                & relevantRange_y .~ atLeastInterval (y₀...y₁)
                                & occlusiveness .~ 4
   where plotWeb graSpec = do
            pixRendered <- pixRender
            pure . mkPlot $ 
              (Dia.image $ Dia.DImage
                            (Dia.ImageRaster $ JPix.ImageRGBA8 pixRendered)
                            renderWidth renderHeight
                            placement)
         cartesianed = sampleEntireWeb_2Dcartesian_lin web renderWidth renderHeight
         renderWidth = 120 -- xResolution graSpec
         renderHeight = 90 -- yResolution graSpec
         x₀ = minimum (fst<$>pts)
         x₁ = maximum (fst<$>pts)
         y₀ = minimum (snd<$>pts)
         y₁ = maximum (snd<$>pts)
         pts = fst . fst <$> Hask.toList (localFocusWeb web)
         xc = (x₀+x₁)/2
         yc = (y₀+y₁)/2
         wPix = (x₁ - x₀)/renderWidth
         hPix = (y₁ - y₀)/renderHeight
         placement = Dia.translation (xc^&yc) <> Dia.scalingX wPix <> Dia.scalingY hPix
         pixRender = do
              seed <- Random.mkStdGen <$> Random.stdUniform
              return $ runST (do
                 randomGen <- newSTRef seed
                 cursorState <- newSTRef (0, NE.fromList $ reverse cartesianed)
                 JPix.withImage renderWidth renderHeight $ \_ix iy -> do
                      (iyPrev, (y, xvs) NE.:| yvs) <- readSTRef cursorState
                      vc <- if iy > iyPrev
                        then case yvs of
                              ((y',(_x,vc):xvs') : yvs') -> do
                                 writeSTRef cursorState (iy, (y', xvs') NE.:| yvs')
                                 return vc
                        else case xvs of
                              ((_x,vc) : xvs') -> do
                                 writeSTRef cursorState (iy, (y, xvs') NE.:| yvs)
                                 return vc
                      rg <- readSTRef randomGen
#if MIN_VERSION_random_fu(0,3,0)
                      let (c, rg') = pureRVar (toRGBA vc) rg
#else
                      let (c, rg') = Random.sampleState (toRGBA vc) rg
#endif
                      writeSTRef randomGen rg'
                      return c
               )
                               


-- | Combine multiple objects in a single plot. Each will get an individual 'tint'
--   (if applicable). This is also the default behaviour of 'Graphics.Dynamic.Plot.R2.Gtk.plotWindow'.
-- 
--   To plot a family objects all with the /same/ (but automatically-chosen) tint,
--   simply use 'plot' on the list, or combine them monoidally with '<>'.
plotMultiple :: Plottable x => [x] -> DynamicPlottable
plotMultiple = fold . chooseAutoTints . map plot

instance (Plottable x) => Plottable (Latest x) where
  plot (Latest (ev₀ :| [])) = plot ev₀
  plot (Latest (ev₀ :| ev₁:evs))
     = plot ev₀ & futurePlots .~ (const . Just . plot . Latest $ ev₁:|evs)

-- | Lazily consume the list, always plotting the latest value available as they
--   arrive.
--   Useful for displaying results of expensive computations that iteratively improve
--   some result, but also for making simple animations (see 'plotDelay').
plotLatest :: Plottable x => [x] -> DynamicPlottable
plotLatest (x:xs) = plot $ Latest (x:|xs)
plotLatest l = plot l


instance Plottable (SimpleTree P2) where
  plot (GenericTree Nothing) = plot ([] :: [SimpleTree P2])
  plot (GenericTree (Just (ctr, root)))
           = def
              & relevantRange_x .~ atLeastInterval xRange
              & relevantRange_y .~ atLeastInterval yRange
              & autoTint
              & axesNecessity .~ 1
              & dynamicPlot .~ plot
   where plot _ = pure . mkPlot $ go 4 ctr (treeBranches root)
          where go w bctr = foldMap (\(c,GenericTree b)
                                       -> autoDashLine w bctr c
                                          <> go (w*0.6) c b     )
         (xRange, yRange) = let allPoints = gPts tree
                                (xmin,xmax) = (minimum&&&maximum) $ (^._x) <$> allPoints
                                (ymin,ymax) = (minimum&&&maximum) $ (^._y) <$> allPoints
                            in (xmin ... xmax, ymin ... ymax)
          where gPts (GenericTree brchs) = foldr (\(c,b) r -> c : gPts b ++ r) [] brchs
         tree = GenericTree [(ctr,root)]
instance Plottable (Trees P2) where
  plot (GenericTree ts) = plot $ (GenericTree . Just) <$> ts
instance Plottable (Trees R2) where
  plot = plot . fmap Dia.P

instance Plottable (SimpleTree (R,R)) where
  plot = plot . fmap (\(x,y) -> DiaTypes.p2 (x,y))
instance Plottable (Trees (R,R)) where
  plot (GenericTree ts) = plot $ (GenericTree . Just) <$> ts

instance Plottable (SimpleTree (R`WithAny`R)) where
  plot = plot . fmap (\(WithAny y x) -> DiaTypes.p2 (x,y))
instance Plottable (Trees (R`WithAny`R)) where
  plot (GenericTree ts) = plot $ (GenericTree . Just) <$> ts

pixelDim :: GraphWindowSpecR2 -> (R, R)
pixelDim grWS = ( graphWindowWidth grWS / fromIntegral (xResolution grWS)
                , graphWindowHeight grWS / fromIntegral (yResolution grWS) )

singlePointFor :: GraphWindowSpecR2 -> P2 -> PlainGraphicsR2
singlePointFor spec = Dia.place circ
 where (pxw,pxh) = pixelDim spec
       circ = Dia.circle 1 & Dia.scaleX pxw & Dia.scaleY pxh


moveStepRel :: (R, R)  -- ^ Relative translation @(Δx/w, Δy/h)@.
            -> (R, R)  -- ^ Relative zoom.
            -> GraphWindowSpec -> GraphWindowSpec
moveStepRel (δx,δy) (ζx,ζy) (GraphWindowSpecR2 l r b t xRes yRes clSchm)
  = GraphWindowSpecR2 l' r' b' t' xRes yRes clSchm
 where qx = (r-l)/2                  ; qy = (t-b)/2
       mx'= l + qx*(1+δx)            ; my'= b + qy*(1+δy) 
       qx'= zoomSafeGuard mx' $ qx/ζx; qy'= zoomSafeGuard my' $ qy/ζy
       l' = mx' - qx'                ; b' = my' - qy'
       r' = mx' + qx'                ; t' = my' + qy'
       zoomSafeGuard m = max (1e-250 + abs m*1e-6) . min 1e+250

graphWindowWidth, graphWindowHeight :: GraphWindowSpec -> R
graphWindowWidth grWS = rBound grWS - lBound grWS
graphWindowHeight grWS = tBound grWS - bBound grWS







instance Semigroup Plot where
  Plot a1 d1 <> Plot a2 d2 = Plot (a1<>a2) (d1<>d2)
instance Monoid Plot where
  mempty = Plot mempty mempty
  mappend = (<>)

mkPlot :: PlainGraphicsR2 -> Plot
mkPlot = Plot mempty

mkAnnotatedPlot :: [Annotation] -> PlainGraphicsR2 -> Plot
mkAnnotatedPlot ans = Plot ans

instance Semigroup DynamicPlottable where
  DynamicPlottable rx₁ ry₁ vpc₁ tm₁ oc₁ ax₁ dl₁ le₁ al₁ fu₁ dp₁
    <> DynamicPlottable rx₂ ry₂ vpc₂ tm₂ oc₂ ax₂ dl₂ le₂ al₂ fu₂ dp₂
        = DynamicPlottable
   (rx₁<>rx₂) (ry₁<>ry₂) (vpc₁.vpc₂) (tm₁++tm₂)
          (oc₁+oc₂) (ax₁+ax₂) (max dl₁ dl₂)
          (le₁++le₂) (al₁++al₂)
          ((<>)<$>fu₁<*>fu₂) (liftA2(liftA2(<>))<$>dp₁<*>dp₂) 
instance Monoid DynamicPlottable where
  mempty = DynamicPlottable
             mempty  -- don't request any range
             mempty
             id      -- don't enforce anything about the viewport
             []      -- no colours
             0       -- neither obscures anything nor has details that could be obscured
             0       -- don't need axis (but don't mind them either)
             (1/20)  -- 20 fps is at the moment the fastest enabled refresh rate anyway
             []      -- no legend entries
             []      -- no axis labels
             mempty  -- no time-evolution
             (const . const $ pure mempty)
  mappend = (<>)
instance Default DynamicPlottable where def = mempty



-- | Set the caption for this plot object that should appear in the
--   plot legend.
legendName :: String -> DynamicPlottable -> DynamicPlottable
legendName n obj = sustained legendEntries %~ (LegendEntry (PlainText n) colour mempty :)
                   $ obj
 where colour = case obj^.inherentColours of
          (c₀:_) -> Just c₀
          _ -> Nothing

-- | Colour this plot object in a fixed shade.
tint :: DCol.Colour ℝ -> DynamicPlottable -> DynamicPlottable
tint col = sustained inherentColours .~ [TrueColour col]
       >>> allDynamicPlot %~ fmap (fmap $ getPlot %~ Dia.lc col . Dia.fc col)
       >>> sustained legendEntries %~ map
                       (plotObjRepresentativeColour ?~ TrueColour col)

-- | Allow the object to be automatically assigned a colour that's otherwise
--   unused in the plot. (This is the default for most plot objects.)
autoTint :: DynamicPlottable -> DynamicPlottable
autoTint = sustained inherentColours .~ []

-- | Assign each object an individual colour, if applicable.
chooseAutoTints :: [DynamicPlottable] -> [DynamicPlottable]
chooseAutoTints = go defaultColourSeq
 where go (c:cs) (o:os)
        | null $ o^.inherentColours
               = (o & sustained inherentColours.~[SymbolicColour c]
                    & allDynamicPlot %~
                       (\plotF gwSpec ->
                          let ac = asAColourWith (colourScheme gwSpec) c
                          in fmap (getPlot %~ Dia.lcA ac . Dia.fcA ac) $ plotF gwSpec)
                    & sustained legendEntries %~ map
                       (plotObjRepresentativeColour ?~ SymbolicColour c)
                 ) : go cs os
       go cs (o:os) = o : go cs os
       go _ [] = []
       


instance (Ord r) => Semigroup (RangeRequest r) where
  MustBeThisRange r <> _ = MustBeThisRange r
  _ <> MustBeThisRange r = MustBeThisRange r
  OtherDimDependantRange r1 <> OtherDimDependantRange r2 = OtherDimDependantRange $ r1<>r2
instance (Ord r) => Monoid (RangeRequest r) where
  mempty = OtherDimDependantRange $ const Nothing
  mappend = (<>)

otherDimDependence :: (Interval r->Interval r) -> RangeRequest r
otherDimDependence = OtherDimDependantRange . fmap

atLeastInterval :: Interval r -> RangeRequest r
atLeastInterval = atLeastInterval' . pure

atLeastInterval' :: Maybe (Interval r) -> RangeRequest r
atLeastInterval' = OtherDimDependantRange . const




                
-- | Render a single view of a collection of plottable objects. This can be
--   used the same way as 'Graphics.Dynamic.Plot.R2.Gtk.plotWindow', but does not open any GTK but gives
--   the result as-is.
--
--   If the objects contain animations, only the initial frame will be rendered.
plotPrerender :: ViewportConfig -> [DynamicPlottable] -> IO PlainGraphicsR2
plotPrerender vpc [] = plotPrerender vpc [dynamicAxes]
plotPrerender vpc plotObjs = do
#if MIN_VERSION_random_fu(0,3,0)
   (renderd, _) <- pureRVar ((getPlot%~Dia.lwO defLineWidth)
                          <$>(plotMultiple plotObjs' ^. dynamicPlotWithAxisLabels)
                                   axLabels
                                   viewport)
                     <$> Random.getStdGen
#else
   renderd <- Random.runRVar ((getPlot%~Dia.lwO defLineWidth)
                          <$>(plotMultiple plotObjs' ^. dynamicPlotWithAxisLabels)
                                   axLabels
                                   viewport)
                             Random.StdRandom
#endif
   annot <- renderAnnotationsForView viewport (renderd^.plotAnnotations)
   return $ annot <> renderd^.getPlot
          & case vpc^.prerenderScaling of
             ValuespaceScaling ->
              Dia.withEnvelope (Dia.rect w h
                               & Dia.alignTL
                               & Dia.moveTo (Dia.P $ V2 lBound tBound)
                                   :: PlainGraphicsR2)
             prs -> normaliseView viewport
                >>> (vpc^.graphicsPostprocessing)
                >>> Dia.withEnvelope (Dia.rect 2 2 :: PlainGraphicsR2)
                >>> case prs of
              NormalisedScaling -> id
              OutputCoordsScaling -> Dia.translate (1 ^& (-1))
                                 >>> Dia.scaleX (fromInt xResolution / 2)
                                 >>> Dia.scaleY (fromInt yResolution / 2)
                                 >>> case vpc^.plotBackground of
                                      Nothing -> id
                                      Just bgc -> Dia.bg bgc
 where viewport@(GraphWindowSpecR2{..}) = autoDefaultView vpc plotObjs'
       w = rBound - lBound; h = tBound - bBound
       plotObjs' = plotObjs ++ if axesNeed>0
                                then [dynamicAxes]
                                else []
       axLabels = concat $ _axisLabelRequests<$>plotObjs
       axesNeed = sum $ _axesNecessity<$>plotObjs

-- | Render the legend (if any) belonging to a collection of plottable objects.
plotLegendPrerender :: LegendDisplayConfig -> [DynamicPlottable]
                               -> IO (Maybe PlainGraphicsR2)
plotLegendPrerender ldc [] = pure Nothing
plotLegendPrerender ldc l = prerenderLegend (TextTK defaultTxtStyle 10 1 0.2 0.2)
                          colourScheme ldc entries
 where tintedl = chooseAutoTints l
       entries = (^.legendEntries) =<< tintedl
       GraphWindowSpecR2{..} = autoDefaultView def $ tintedl



objectPlotterThread :: DynamicPlottable
                       -> MVar (GraphWindowSpec, [AxisLabel])
                       -> MVar (Interactions (ℝ,ℝ))
                       -> MVar (GraphWindowSpec, AnnotPlot)
                       -> IO ()
objectPlotterThread pl₀ viewVar mouseVar diaVar = loop Nothing pl₀ where
 loop lastMousePressed pl = do
    tPrev <- getCurrentTime
    (view, labels) <- readMVar viewVar
    newMice <- tryTakeMVar mouseVar
    let mice = case (newMice, lastMousePressed) of
          (Just m, _) -> m
          (Nothing, p) -> Interactions [] p
    diagram <- evaluate . (getPlot %~ Dia.lwO defLineWidth)
#if MIN_VERSION_random_fu(0,3,0)
                      . fst
                =<< pureRVar
                 ((pl^.dynamicPlotWithAxisLabels) labels view)
                     <$> Random.getStdGen
#else
                =<< Random.runRVar
                 ((pl^.dynamicPlotWithAxisLabels) labels view)
                 Random.StdRandom
#endif
    putMVar diaVar (view, (diagram, (pl^.legendEntries, pl^.axisLabelRequests)))
    waitTill $ addUTCTime (pl^.frameDelay) tPrev
    loop (_currentDragEndpoints mice) $ case pl^.futurePlots $ mice of
       Just pl' -> pl'
       Nothing  -> pl
    
    
normaliseView :: GraphWindowSpecR2 -> PlainGraphicsR2 -> PlainGraphicsR2
normaliseView currentView@(GraphWindowSpecR2{..})
      = (Dia.scaleX xUnZ :: PlainGraphicsR2->PlainGraphicsR2)
                   . Dia.scaleY yUnZ
                 . Dia.translate (Dia.r2(-x₀,-y₀))
   where xUnZ = 1/w; yUnZ = 1/h
         w = (rBound - lBound)/2; h = (tBound - bBound)/2
         x₀ = lBound + w; y₀ = bBound + h

-- | Require that both coordinate axes are zoomed the same way, such that e.g.
--   the unit circle will appear as an actual circle.
unitAspect :: DynamicPlottable
unitAspect = def & viewportConstraint . mapped . windowDataAspect .~ 1


autoDefaultView :: ViewportConfig -> [DynamicPlottable] -> GraphWindowSpec
autoDefaultView vpConf graphs =
         foldr _viewportConstraint
            (GraphWindowSpecR2 l r b t
                               (vpConf^.xResV) (vpConf^.yResV) defaultColourScheme)
            graphs
  where (xRange, yRange) = foldMap (_relevantRange_x &&& _relevantRange_y) graphs
        ((l,r), (b,t)) = ( xRange `dependentOn` yRange
                         , yRange `dependentOn` xRange )
        
        dependentOn :: RangeRequest R -> RangeRequest R -> (R,R)
        MustBeThisRange (Interval a b) `dependentOn` _ = (a,b)
        OtherDimDependantRange ξ `dependentOn` MustBeThisRange i
           = addMargin . defRng . ξ $ pure i
        OtherDimDependantRange ξ `dependentOn` OtherDimDependantRange υ
           = addMargin . defRng . ξ . pure . defRng $ υ Nothing
        
        defRng (Just (Interval a b)) | b>a     
                  = Interval a b
        defRng _  = Interval (-1) 1   -- ad-hoc hack to catch NaNs etc..
        addMargin (Interval a b) = (a - q, b + q)
            where q = (b - a) * (1/(vpConf^.plotContentZoomFactor) - 1)
  

renderAnnotationsForView :: GraphWindowSpecR2 -> [Annotation] -> IO PlainGraphicsR2
renderAnnotationsForView viewport@GraphWindowSpecR2{..}
           = fmap mconcat . mapM (prerenderAnnotation antTK)
 where antTK = DiagramTK { viewScope = viewport 
                         , textTools = TextTK defaultTxtStyle txtSize aspect 0.2 0.2 }
       txtSize = h * fontPts / fromIntegral yResolution
       aspect  = w * fromIntegral yResolution
                               / (h * fromIntegral xResolution)
       w = (rBound - lBound)/2; h = (tBound - bBound)/2
       fontPts = 12





-- | Plot an (assumed continuous) function in the usual way.
--   Since this uses functions of actual 'Double' values, you have more liberty
--   of defining functions with range-pattern-matching etc., which is at the moment
--   not possible in the ':-->' category.
-- 
--   However, because 'Double' can't really prove properties of a mathematical
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
continFnPlot f = def
             & relevantRange_y .~ otherDimDependence yRangef
             & autoTint
             & axesNecessity .~ 1
             & dynamicPlot .~ pure . plot
 where yRangef = onInterval $ \(l, r) -> ((!%0.1) &&& (!%0.9)) . sort . pruneOutlyers
                                               $ map f [l, l + (r-l)/80 .. r]
       plot (GraphWindowSpecR2{..}) = curve `deepseq` mkPlot (trace curve)
        where δx = (rBound - lBound) * 2 / fromIntegral xResolution
              curve = [ (x ^& f x) | x<-[lBound, lBound+δx .. rBound] ]
              trace (p:q:ps) = (if not $ any (isNaN . (^._y)) [p, q]
                                 then simpleLine p q else mempty
                                ) <> trace (q:ps)
              trace _ = mempty
       pruneOutlyers = filter (not . isNaN) 
       l!%η = case length l of
         ll | ll<2      -> error
                 "Function appears to yield NaN most of the time. Cannot be plotted."
            | otherwise -> l !! floor (fromIntegral ll * η)


-- | Plot a function that assigns every point in view a colour value.
--
-- @
-- > plotWindow [colourPaintPlot $ \(x,y) -> case (x^2+y^2, atan2 y x) of (r,φ) -> guard (sin (7*φ-2*r) > r) >> Just (Dia.blend (tanh r) Dia.red Dia.green), unitAspect ]
-- @
-- 
--   <<images/examples/propeller.png>>
-- 
--   We try to evaluate that function no more often than necessary, but since it's a
--   plain function with no differentiability information there's only so much that can
--   be done; this requires a tradeoff between rasterisation fineness and performance.
--   It works well for simple, smooth functions, but may not be adequate for
--   functions with strong edges/transients, nor for expensive to compute functions.
colourPaintPlot :: ((Double,Double) -> Maybe (DCol.Colour Double)) -> DynamicPlottable
colourPaintPlot f = def & dynamicPlot .~ pure . plot
                        & axesNecessity .~ 0.5
 where plot graSpec = mkPlot . Dia.image $ Dia.DImage
                            (Dia.ImageRaster $ JPix.ImageRGBA8 pixRendered)
                            renderWidth renderHeight
                            placement
        where preSeekWidth = round $ fromIntegral (xResolution graSpec) / 12
              preSeekHeight = round $ fromIntegral (yResolution graSpec) / 12
              roughRenderWidth = fromIntegral preSeekWidth * 2 - 1
              roughRenderHeight = fromIntegral preSeekHeight * 2 - 1
              renderWidth, renderHeight :: Num n => n
              renderWidth = fromIntegral roughRenderWidth * 2 - 1
              renderHeight = fromIntegral roughRenderHeight * 2 - 1
              x₀ = lBound graSpec
              x₁ = rBound graSpec
              y₀ = bBound graSpec
              y₁ = tBound graSpec
              xc = (x₀+x₁)/2
              yc = (y₀+y₁)/2
              wPix = (x₁ - x₀)/renderWidth
              wRoughPix = (x₁+wPix - x₀)/fromIntegral roughRenderWidth
              wPreSeekPix = (x₁+wRoughPix - x₀)/fromIntegral preSeekWidth
              hPix = (y₁ - y₀)/renderHeight
              hRoughPix = (y₁+hPix - y₀)/fromIntegral roughRenderHeight
              hPreSeekPix = (y₁+hRoughPix - y₀)/fromIntegral preSeekHeight
              placement
                  = Dia.translation (xc^&yc) <> Dia.scalingX wPix <> Dia.scalingY hPix
              
              pixRendered = runST (do
                 
                 hits <- newSTRef ([] :: [(Int,Int)])
                 
                 rough² <- JPix.withImage preSeekWidth preSeekHeight
                  `id`\ix iy -> do
                        let x = x₀ + wPreSeekPix*fromIntegral ix
                            y = y₁ - hPreSeekPix*fromIntegral iy
                        case f (x,y) of
                            Just fxy -> do
                              modifySTRef hits ((ix,iy):)
                              pure `id` JPix.promotePixel
                                          (CSp.quantiseColour fxy :: JPix.PixelRGB8)
                            Nothing ->
                              pure `id` JPix.PixelRGBA8 0 0 0 0
                 
                 allHits <- readSTRef hits
                 
                 let (intermediate, gradientHotSpots)
                      = refiningScaleX2Bilinear allHits
                         (\(ix,iy) -> case f ( x₀ + wRoughPix*fromIntegral ix
                                             , y₁ - hRoughPix*fromIntegral iy ) of
                           Just fxy -> JPix.promotePixel
                                    (CSp.quantiseColour fxy :: JPix.PixelRGB8)
                           Nothing -> JPix.PixelRGBA8 0 0 0 0 )
                         rough²
                 
                 pure . fst . refiningScaleX2Bilinear gradientHotSpots
                         (\(ix,iy) -> case f ( x₀ + wPix*fromIntegral ix
                                             , y₁ - hPix*fromIntegral iy ) of
                           Just fxy -> JPix.promotePixel
                                    (CSp.quantiseColour fxy :: JPix.PixelRGB8)
                           Nothing -> JPix.PixelRGBA8 0 0 0 0 )
                         $ intermediate
                )

type (-->) = RWDiffable ℝ

-- | Plot a continuous function in the usual way, taking arguments from the
--   x-Coordinate and results to the y one.
--   The signature looks more complicated than it is; think about it as requiring
--   a polymorphic 'Floating' function. Any simple expression like
--   @'fnPlot' (\\x -> sin x / cos (sqrt x))@ will work.
--   
--   Under the hood this uses the category of region-wise differentiable functions,
--   'RWDiffable', to prove that no details are omitted (like small high-frequency
--   bumps). Note that this can become difficult for contrived cases like @cos(1/sin x)@
--   – while such functions will never come out with aliasing artifacts, they also
--   may not come out quickly at all. (But for well-behaved functions, using the
--   differentiable category actually tends to be more effective, because the algorithm
--   immediately sees when it can describe an almost-linear region with only a few line
--   segments.)
-- 
--   This function is equivalent to using 'plot' on an 'RWDiffable' arrow.
fnPlot :: (∀ m . Object (RWDiffable ℝ) m
                         => AgentVal (-->) m ℝ -> AgentVal (-->) m ℝ )
                     -> DynamicPlottable
fnPlot f = plot fd
 where fd :: ℝ --> ℝ
       fd = alg f

uncertainFnPlot :: ∀ m . (SimpleSpace m, Scalar m ~ ℝ)
                 => (ℝ -> (m +> ℝ)) -> Shade' m -> DynamicPlottable
uncertainFnPlot = case linearManifoldWitness :: LinearManifoldWitness m of
   LinearManifoldWitness
#if !MIN_VERSION_manifolds(0,6,0)
     BoundarylessWitness
#endif
     -> \mfun (Shade' mBest me)
      -> plot $ continFnPlot (($ mBest) . mfun)
            : [ tweakPrerendered (Dia.opacity 0.2)
               $ continFnPlot (($ mBest^+^σ*^δm) . mfun)
              | δm <- normSpanningSystem' me
              , σ <- [-1,1] ]

linregressionPlot :: ∀ x m y . ( SimpleSpace m, Scalar m ~ ℝ, y ~ ℝ, x ~ ℝ )
                 =>  (x -> (m +> y)) -> [(x, Shade' y)]
                      -> (Shade' m -> DynamicPlottable -> DynamicPlottable
                               -> DynamicPlottable)
                         -> DynamicPlottable
linregressionPlot = lrp (linearManifoldWitness, dualSpaceWitness)
 where lrp :: (LinearManifoldWitness m, DualSpaceWitness m)
                 -> (x -> (m +> y)) -> [(x, Shade' y)]
                      -> (Shade' m -> DynamicPlottable -> DynamicPlottable
                              -> DynamicPlottable)
                        -> DynamicPlottable
       lrp _ _ [] _ = mempty
       lrp (LinearManifoldWitness
#if !MIN_VERSION_manifolds(0,6,0)
                 BoundarylessWitness
#endif
              , DualSpaceWitness)
             mfun dataPts resultHook = resultHook shm
                        (plot [ plot (Shade (x,y) (sumSubspaceNorms mempty $ dualNorm ey)
                                        :: Shade (ℝ,ℝ))
                              | (x, Shade' y ey) <- dataPts ])
                        (uncertainFnPlot mfun shm)
        where mBest = linearFit_bestModel regResult
              regResult = linearRegression (mfun . (bcx.+~^))
                                  [ (δx,(fromInterior y,ey))
                                  | (x,Shade' y ey)<-dataPts
                                  , let Just δx = x.-~.bcx ]
              Just bcx = (pointsBarycenter . NE.fromList $ fst<$>dataPts)
              shm :: Shade' m
              shm = Shade' mBest $ linearFit_modelUncertainty regResult

-- | Plot a continuous, “parametric function”, i.e. mapping the real line to a path in ℝ².
paramPlot :: (∀ m . ( WithField ℝ PseudoAffine m, SimpleSpace (Needle m) )
                       => AgentVal (-->) m ℝ -> (AgentVal (-->) m ℝ, AgentVal (-->) m ℝ) )
                     -> DynamicPlottable
paramPlot f = plot fd
 where fd :: ℝ --> (ℝ,ℝ)
       fd = alg1to2 f


scrutiniseDiffability :: (∀ m . ( WithField ℝ PseudoAffine m
                                , SimpleSpace (Needle m) )
                         => AgentVal (-->) m ℝ -> AgentVal (-->) m ℝ )
                     -> DynamicPlottable
scrutiniseDiffability f = plot [{-plot fd, -}dframe 0.2, dframe 0.02]
 where fd :: ℝ --> ℝ
       fd = alg f
       fscrut = analyseLocalBehaviour fd
       dframe rfh = def
                 & autoTint
                 & dynamicPlot .~ pure . mkFrame
        where mkFrame (GraphWindowSpecR2{..}) = case fscrut xm of
                      Just ((ym,y'm), δOδx²)
                        | Just δx <- δOδx² δy
                          -> δx `seq` let frame = mconcat
                                            [ simpleLine ((xm-δx)^&(ym+yo-δx*y'm))
                                                         ((xm+δx)^&(ym+yo+δx*y'm))
                                            | yo <- [-δy, δy] ]
                                      in mkPlot frame
                        | otherwise
                          -> y'm `seq` mkPlot
                             ( autoDashLine 0.5 ((xm-δxdef)^&(ym-δxdef*y'm))
                                                ((xm+δxdef)^&(ym+δxdef*y'm))  )
                      _ -> mempty
               where xm = (rBound + lBound) / 2
                     δxdef = (rBound - lBound) / 10
                     δy = rfh * (tBound - bBound)
              
                                 

continColourSurfaceFnPlot :: ((Double,Double) -> DCol.Colour Double) -> DynamicPlottable
continColourSurfaceFnPlot f = def
             & axesNecessity .~ 1
             & occlusiveness .~ 4
             & dynamicPlot .~ plot
 where plot (GraphWindowSpecR2{..}) = pure . mkPlot
              $ Dia.place
                ( Dia.rasterDia cf (xResolution`div`4) (yResolution`div`4)
                  & Dia.scaleX wPix & Dia.scaleY hPix
                ) ( ((lBound+rBound-wPix)/2) ^& ((tBound+bBound+hPix)/2) )
        where cf i j = f ( lBound + wPix * fromIntegral i, tBound - hPix * fromIntegral j )
                        `Dia.withOpacity` 0.2
              w = rBound - lBound; h = tBound - bBound
              wPix = w*4 / fromIntegral xResolution
              hPix = h*4 / fromIntegral yResolution

data AxesStyle = DynamicAxesStyle
data DynamicAxes = DynamicAxes { yAxisClasses, xAxisClasses :: [AxisClass] }
data AxisClass = AxisClass { visibleAxes :: [Axis], axisStrength :: Double, decPrecision :: Int }
data Axis = Axis { axisPosition :: R }

crtDynamicAxes :: GraphWindowSpec -> DynamicAxes
crtDynamicAxes (GraphWindowSpecR2 {..}) = DynamicAxes yAxCls xAxCls
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
                     luDSdiv = ll . takeWhile (\d -> pixelScale * minSpc < 1/d )
                                      . join $ iterate (map(*10)) [1, 2, 5]
                     ll [] = error $ "pixelScale = "++show pixelScale
                                   ++"; minSpc = "++show minSpc
                     ll l = last l
       lvlSpecs = [ (80, 0.3), (18, 0.1) ]



-- | Coordinate axes with labels. For many plottable objects, these will be added
--   automatically, by default (unless inhibited with 'noDynamicAxes').
dynamicAxes :: DynamicPlottable
dynamicAxes = def
             & axesNecessity .~ superfluent
             & occlusiveness .~ 1
             & dynamicPlotWithAxisLabels .~ \lbls -> pure . plot lbls
 where plot poLabels gwSpec@(GraphWindowSpecR2{..}) = Plot (dirLabels++tickLabels) lines
        where (DynamicAxes yAxCls xAxCls) = crtDynamicAxes gwSpec
              lines = zeroLine (lBound^&0) (rBound^&0)  `provided`(bBound<0 && tBound>0)
                   <> zeroLine (0^&bBound) (0^&tBound)  `provided`(lBound<0 && rBound>0)
                   <> foldMap (renderClass $ \x -> (x^&bBound, x^&tBound)) yAxCls
                   <> foldMap (renderClass $ \y -> (lBound^&y, rBound^&y)) xAxCls
              tickLabels
                     = do (dirq, hAlign, vAlign, acl) <- zip4 [\x -> x^&0, \y -> 0^&y ] 
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
              dirLabels = [ if dir^._x > dir^._y
                             then Annotation (TextAnnotation txt
                                              $ TextAlignment AlignMid AlignBottom)
                                             (ExactPlace $ xFar^&0)
                                             False
                             else Annotation (TextAnnotation txt
                                              $ TextAlignment AlignBottom AlignMid)
                                             (ExactPlace $ 0^&yFar)
                                             False
                          | (dir,lbl) <- poLabels
                          , let txt = PlainText lbl
                                xFar = if rBound > abs lBound/2
                                        then rBound else lBound
                                yFar = if tBound > abs bBound/2
                                        then tBound else bBound
                          ]
       zeroLine p1 p2 = simpleLine' defLineWidth p1 p2 & Dia.lc Dia.grey
       renderClass crd (AxisClass axes strength _)
          = foldMap (uncurry (simpleLine' defLineWidth) . crd . axisPosition) axes
             & Dia.lcA (Dia.grey `DCol.withOpacity` strength)


noDynamicAxes :: DynamicPlottable
noDynamicAxes = def & axesNecessity .~ superfluent

xAxisLabel :: String -> DynamicPlottable
xAxisLabel str = def & axisLabelRequests .~ [(1^&0, str)]
                     & axesNecessity .~ 2

yAxisLabel :: String -> DynamicPlottable
yAxisLabel str = def & axisLabelRequests .~ [(0^&1, str)]
                     & axesNecessity .~ 2


simpleLine :: P2 -> P2 -> PlainGraphicsR2
simpleLine p q = Dia.fromVertices [p,q]

defLineWidth :: Double
defLineWidth = 2

simpleLine' :: Double -> P2 -> P2 -> PlainGraphicsR2
simpleLine' w p q = Dia.fromVertices [p,q] & Dia.lwO w

autoDashLine :: Double -> P2 -> P2 -> PlainGraphicsR2
autoDashLine w p q = simpleLine' (max 1 w) p q
       & if w < 1 then Dia.dashingO [w*6, 3] 0 else id


tweakPrerendered :: (PlainGraphicsR2->PlainGraphicsR2) -> DynamicPlottable->DynamicPlottable
tweakPrerendered f = allDynamicPlot %~ (fmap tweak .)
 where tweak = getPlot %~ f

opacityFactor :: Double -> DynamicPlottable -> DynamicPlottable
opacityFactor = tweakPrerendered . Dia.opacity


-- | When you &#x201c;plot&#x201d; 'xInterval' / 'yInterval', it is ensured that the (initial) view encompasses 
-- (at least) the specified range.
-- Note there is nothing special about these &#x201c;flag&#x201d; objects: /any/ 'Plottable' can request a 
-- certain view, e.g. for a discrete point cloud it's obvious and a function defines at least
-- a @y@-range for a given @x@-range. Only use explicit range when necessary.
xInterval :: (Double, Double) -> DynamicPlottable

-- | Like 'xInterval', this only affects what range is plotted. However, it doesn't merely
--   request that a certain interval /should be visible/, but actually enforces particular
--   values for the left and right boundary. Nothing outside the range will be plotted
--   (unless there is another, contradicting 'forceXRange').
forceXRange :: (Double, Double) -> DynamicPlottable

yInterval, forceYRange :: (Double, Double) -> DynamicPlottable

xInterval (l,r) = mempty & relevantRange_x .~ atLeastInterval (Interval l r)
forceXRange (l,r) = mempty & relevantRange_x .~ MustBeThisRange (Interval l r)
yInterval (b,t) = mempty & relevantRange_y .~ atLeastInterval (Interval b t)
forceYRange (b,t) = mempty & relevantRange_y .~ MustBeThisRange (Interval b t)
 

-- | Disregard this object when computing what coordinate range the plot should
--   include. In other words, the visible range is determined by other objects
--   to be plotted, and this object will only be visible inasmuch it happens to be
--   in view in the space occupied by the other objects.
ignoreExtent :: DynamicPlottable -> DynamicPlottable
ignoreExtent plo = plo & relevantRange_x .~ OtherDimDependantRange (const Nothing)
                       & relevantRange_y .~ OtherDimDependantRange (const Nothing)
                       & viewportConstraint .~ id
                       & futurePlots %~ fmap (fmap ignoreExtent)



-- $interactiveExplanation
--   'MouseClicks', 'ViewXCenter', 'ViewYResolution' etc. can be used as arguments to some object
--   you 'plot', if you want to plot stuff that depends on user interaction
--   or just on the screen's visible range, for instance to calculate a tangent
--   at the middle of the screen:
-- 
-- @
-- plotWindow [fnPlot sin, plot $ \\(ViewXCenter xc) x -> sin xc + (x-xc) * cos xc]
-- @
-- 
--   <<images/examples/sin-ctrd-tangents.gif>>
newtype ViewXCenter = ViewXCenter { getViewXCenter :: Double }
instance (Plottable p) => Plottable (ViewXCenter -> p) where
  plot f = def & relevantRange_y .~ OtherDimDependantRange
                     (\g -> deescalate relevantRange_y g . plot . f . cxI =<< g)
               & inherentColours .~ fcxVoid^.inherentColours
               & axesNecessity .~ fcxVoid^.axesNecessity
               & dynamicPlotWithAxisLabels .~
                     \lbls g -> _dynamicPlotWithAxisLabels (plot . f $ cx g) lbls g
    where cx (GraphWindowSpecR2{..}) = ViewXCenter $ (lBound+rBound)/2
          cxI (Interval l r) = ViewXCenter $ (l+r)/2
          fcxVoid = plot . f $ ViewXCenter 0.23421  -- Yup, it's magic.
          deescalate rfind otherdim p = case p^.rfind of
             MustBeThisRange i -> pure i
             OtherDimDependantRange ifr -> ifr otherdim
newtype ViewYCenter = ViewYCenter { getViewYCenter :: Double }
instance (Plottable p) => Plottable (ViewYCenter -> p) where
  plot f = def & relevantRange_x .~ OtherDimDependantRange
                     (\g -> deescalate relevantRange_x g . plot . f . cyI =<< g)
               & inherentColours .~ fcyVoid^.inherentColours
               & axesNecessity .~ fcyVoid^.axesNecessity
               & dynamicPlotWithAxisLabels .~
                     \lbls g -> _dynamicPlotWithAxisLabels (plot . f $ cy g) lbls g
    where cy (GraphWindowSpecR2{..}) = ViewYCenter $ (bBound+tBound)/2
          cyI (Interval b t) = ViewYCenter $ (b+t)/2
          fcyVoid = plot . f $ ViewYCenter 0.319421  -- Alright, alright... the idea is to avoid exact equality with zero or any other number that might come up in some plot object, since such an equality can lead to div-by-zero problems.
          deescalate rfind otherdim p = case p^.rfind of
             MustBeThisRange i -> pure i
             OtherDimDependantRange ifr -> ifr otherdim
newtype ViewWidth = ViewWidth { getViewWidth :: Double }
instance (Plottable p) => Plottable (ViewWidth -> p) where
  plot f = def & relevantRange_y .~ OtherDimDependantRange
                     (\g -> deescalate relevantRange_y g . plot . f . wI =<< g)
               & inherentColours .~ fwVoid^.inherentColours
               & axesNecessity .~ fwVoid^.axesNecessity
               & dynamicPlotWithAxisLabels .~
                     \lbls g -> _dynamicPlotWithAxisLabels (plot . f $ w g) lbls g
    where w (GraphWindowSpecR2{..}) = ViewWidth $ rBound - lBound
          wI (Interval l r) = ViewWidth $ r - l
          fwVoid = plot . f $ ViewWidth 2.142349
          deescalate rfind otherdim p = case p^.rfind of
             MustBeThisRange i -> pure i
             OtherDimDependantRange ifr -> ifr otherdim
newtype ViewHeight = ViewHeight { getViewHeight :: Double }
instance (Plottable p) => Plottable (ViewHeight -> p) where
  plot f = def & relevantRange_x .~ OtherDimDependantRange
                     (\g -> deescalate relevantRange_x g . plot . f . hI =<< g)
               & inherentColours .~ fhVoid^.inherentColours
               & axesNecessity .~ fhVoid^.axesNecessity
               & dynamicPlotWithAxisLabels .~
                     \lbls g -> _dynamicPlotWithAxisLabels (plot . f $ h g) lbls g
    where h (GraphWindowSpecR2{..}) = ViewHeight $ tBound - bBound
          hI (Interval b t) = ViewHeight $ t - b
          fhVoid = plot . f $ ViewHeight 1.494213
          deescalate rfind otherdim p = case p^.rfind of
             MustBeThisRange i -> pure i
             OtherDimDependantRange ifr -> ifr otherdim
newtype ViewXResolution = ViewXResolution { getViewXResolution :: Int }
newtype ViewYResolution = ViewYResolution { getViewYResolution :: Int }

newtype MouseClicks = MouseClicks {
      getClickPositions :: [(ℝ,ℝ)] -- ^ A history of all clicks that were done
                                   --   in this window; more specifically, of all
                                   --   /left mouse-button release events/ recorded.
    }
instance (Plottable p) => Plottable (MouseClicks -> p) where
  plot f = go []
   where go oldClicks = addInterrupt . plot . f $ MouseClicks oldClicks
          where addInterrupt :: DynamicPlottable -> DynamicPlottable
                addInterrupt pl = pl
                     & futurePlots %~ \anim -> \case
                         interac@(Interactions [] _) -> fmap addInterrupt $ anim interac
                         Interactions newClicks _ -> pure . go
                              $ (_releaseLocation<$>newClicks) ++ oldClicks

newtype MousePress = MousePress {
      lastMousePressedLocation :: (ℝ,ℝ)
    }
instance (Plottable p) => Plottable (MousePress -> p) where
  plot f = go Nothing
   where go :: Maybe (ℝ,ℝ) -> DynamicPlottable
         go Nothing = mempty & futurePlots .~ pure . \case
              Interactions [] Nothing -> go Nothing
              Interactions (click:_) Nothing -> go . Just $ click^.releaseLocation
              Interactions _ (Just current) -> go . Just $ current^.releaseLocation
         go (Just lastPressed) = addInterrupt . plot . f $ MousePress lastPressed
          where addInterrupt :: DynamicPlottable -> DynamicPlottable
                addInterrupt pl = pl
                     & futurePlots %~ \anim -> \case
                  Interactions [] Nothing -> fmap addInterrupt $ anim mempty
                  Interactions (click:_) Nothing
                                  -> pure . go . Just $ click^.releaseLocation
                  Interactions _ (Just drag)
                                  -> pure . go . Just $ drag^.releaseLocation

newtype MousePressed = MousePressed {
      mouseIsPressedAt :: Maybe (ℝ,ℝ)
    }
instance (Plottable p) => Plottable (MousePressed -> p) where
  plot f = go Nothing
   where go :: Maybe (ℝ,ℝ) -> DynamicPlottable
         go lastInt = addInterrupt . plot . f $ MousePressed lastInt
          where addInterrupt :: DynamicPlottable -> DynamicPlottable
                addInterrupt pl = pl
                     & futurePlots %~ \anim -> \case
                  Interactions _ Nothing
                    | isNothing lastInt  -> fmap addInterrupt $ anim mempty
                    | otherwise          -> return $ go Nothing
                  Interactions _ (Just drag)
                                  -> pure . go . Just $ drag^.releaseLocation

-- | Move through a sequence of plottable objects, switching to the next
--   whenever a click is received anywhere on the screen. Similar to 'plotLatest',
--   but does not proceed automatically.
clickThrough :: Plottable p => [p] -> DynamicPlottable
clickThrough [] = mempty
clickThrough [final] = plot final
clickThrough (v:vs) = addInterrupt $ plot v
 where addInterrupt :: DynamicPlottable -> DynamicPlottable
       addInterrupt pl = pl & futurePlots %~ \anim -> \case
           interac@(Interactions [] _) -> fmap addInterrupt $ anim interac
           Interactions (_:_) _ -> Just $ clickThrough vs

mouseInteractive :: Plottable p
       => (MouseEvent (ℝ,ℝ) -> s -> s)
       -> s
       -> (s -> p)
       -> DynamicPlottable
mouseInteractive upd initl f = go initl
 where go s = addInterrupt . plot $ f s
        where addInterrupt :: DynamicPlottable -> DynamicPlottable
              addInterrupt pl = pl
                   & futurePlots %~ \anim -> \case
                Interactions _ Nothing
                       -> fmap addInterrupt $ anim mempty
                Interactions _ (Just drag)
                       -> pure . go $ upd drag s

indexedT :: Hask.Traversable t => t a -> t (Int,a)
indexedT v = (`evalState`0) . Hask.forM v $ \x -> do
   i <- get
   put $ i+1
   return (i,x)

updateAt :: Hask.Traversable t => Int -> (a->a) -> t a -> t a
updateAt iM f v = (`evalState`0) . Hask.forM v $ \x -> do
   i <- get
   put $ i+1
   return $ if i==iM then f x
                     else x

-- | Plot something dependent on points that the user can interactively move around.
--   The nearest point (Euclidean distance) is always picked to be dragged.
withDraggablePoints :: ∀ p list . (Plottable p, Traversable list)
       => list (ℝ,ℝ) -> (list (ℝ,ℝ) -> p) -> DynamicPlottable
withDraggablePoints pts₀ f = go pts₀
   where go :: list (ℝ,ℝ) -> DynamicPlottable
         go pts = addInterrupt . plot $ f pts
          where addInterrupt :: DynamicPlottable -> DynamicPlottable
                addInterrupt pl = pl
                     & futurePlots %~ \anim -> \case
                  Interactions _ Nothing -> return $ go pts
                  Interactions _ (Just drag)
                    -> let p = drag^.releaseLocation
                           grabbed = fst . minimumBy
                             (comparing $ magnitude . (^-^p) . snd)
                             $ indexedT pts
                       in return . go $ updateAt grabbed (const p) pts


atExtendOf :: PlainGraphicsR2 -> PlainGraphicsR2 -> PlainGraphicsR2
atExtendOf d₁ = atExtendOf' d₁ 1

atExtendOf' :: PlainGraphicsR2 -> Double -> PlainGraphicsR2 -> PlainGraphicsR2
atExtendOf' d₁ q d₂ = d₂
                      & Dia.translate ((pux+plx-lux-llx)/2 ^& (puy+ply-luy-lly)/2)
                      & Dia.scaleX (q*(pux-plx)/(lux-llx))
                      & Dia.scaleY (q*(puy-ply)/(luy-lly))
 where (Just (plx,pux)) = Dia.extentX d₁; (Just (ply,puy)) = Dia.extentY d₁
       (Just (llx,lux)) = Dia.extentX d₂; (Just (lly,luy)) = Dia.extentY d₂



waitTill :: UTCTime -> IO ()
waitTill t = do
   tnow <- getCurrentTime
   threadDelay . max 1000 . round $ diffUTCTime t tnow
                                   * 1e+6 -- threadDelay ticks in microseconds

-- | Limit the refresh / frame rate for this plot object. Useful to slowly
--   study some sequence of plots with 'plotLatest', or to just reduce processor load.
-- 
--   Note: the argument will probably change to
--   <http://hackage.haskell.org/package/thyme-0.3.5.5/docs/Data-Thyme-Clock.html#t:NominalDiffTime NominalDiffTime> from the <http://hackage.haskell.org/package/thyme thyme>
--   library soon.
plotDelay :: NominalDiffTime -> DynamicPlottable -> DynamicPlottable
plotDelay dly = sustained frameDelay .~ dly

-- | Disable an animation, i.e. take an animated plot and show only the first frame.
freezeAnim :: DynamicPlottable -> DynamicPlottable
freezeAnim = futurePlots .~ const Nothing

-- | Wait with starting the animation until the user has clicked on it.
startFrozen :: DynamicPlottable -> DynamicPlottable
startFrozen = futurePlots %~ \f inta -> if inta==mempty
                  then Nothing
                  else f inta


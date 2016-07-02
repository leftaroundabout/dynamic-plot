-- |
-- Module      : Graphics.Dynamic.Plot.R2
-- Copyright   : (c) Justus Sagemüller 2013-2015
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
        , lineSegPlot
        , PlainGraphicsR2
        , shapePlot
        , diagramPlot
        -- ** Legend captions
        , legendName
        -- ** View selection
        , xInterval, yInterval, forceXRange, forceYRange
        -- ** View dependance
        , ViewXCenter(..), ViewYCenter(..), ViewWidth(..), ViewHeight(..)
        , ViewXResolution(..), ViewYResolution(..)
        -- ** Auxiliary plot objects
        , dynamicAxes, noDynamicAxes
        -- ** Plot type
        , DynamicPlottable
        -- ** Legacy
        , PlainGraphics(..)
        ) where

import Graphics.Dynamic.Plot.Colour
import Graphics.Dynamic.Plot.Internal.Types
import Graphics.Text.Annotation



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


import Data.List (foldl', sort, sortBy, partition, zip4)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as Arr
import Data.Maybe
import Data.Semigroup
import Data.Default
import Data.Foldable (fold, foldMap)
import qualified Data.Foldable as Hask
import Data.Function (on)
import Data.Ord (comparing)

import Data.VectorSpace
import Data.Basis
import Data.AffineSpace
import Data.LinearMap.HerMetric
import Data.Manifold.PseudoAffine
import Data.Function.Differentiable
import Data.Manifold.Types
import Data.Manifold.TreeCover
import Data.Manifold.Web
import qualified Data.Map.Lazy as Map

import Data.Tagged

import Text.Printf

import Data.IORef

import System.IO
import System.Exit
import System.Process
import Data.Time





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

instance Plottable PlainGraphics where
  plot (PlainGraphics d) = def {
             relevantRange_x = atLeastInterval rlx
           , relevantRange_y = atLeastInterval rly
           , axesNecessity = -1
           , dynamicPlot = plot
           }
   where bb = DiaBB.boundingBox d
         (rlx,rly) = case DiaBB.getCorners bb of
                       Just (c1, c2)
                        -> ( c1^._x ... c2^._x
                           , c1^._y ... c2^._y )
         plot _ = mkPlot d


-- | Use a generic diagram within a plot.
-- 
--   Like with the various specialised function plotters, this will get automatically
--   tinted to be distinguishable from other plot objects in the same window.
--   Use 'diagramPlot' instead, if you want to view the diagram as-is.
shapePlot :: PlainGraphicsR2 -> DynamicPlottable
shapePlot d = (diagramPlot d) { isTintableMonochromic = True, axesNecessity = 0 }

-- | Plot a generic 'Dia.Diagram'.
diagramPlot :: PlainGraphicsR2 -> DynamicPlottable
diagramPlot d = plot $ PlainGraphics d


  
instance Plottable (R-->R) where
  plot f = def { relevantRange_y = OtherDimDependantRange yRangef
               , isTintableMonochromic = True
               , axesNecessity = 1
               , dynamicPlot = plot }
   where yRangef (Option Nothing) = Option Nothing
         yRangef (Option (Just (Interval l r)))
             = case intervalImages
                      100
                      ( const . metricFromLength $ (r-l)/16 , const $ metricFromLength 0.0001 )
                      ( alg (\x -> ( point l?<x?<point r ?-> (f$~x) ))) of 
                 ([],[]) -> Option Nothing
                 (liv,riv) -> pure . foldr1 (<>) . map (uncurry Interval . snd)
                               $ take 4 liv ++ take 4 riv
         
         plot gs@(GraphWindowSpecR2{..}) = curves `deepseq`
                                          mkPlot (foldMap trace curves)
          where curves :: [[P2]]
                curves = map (map $ convℝ² . snd) . gatherSides
                        $ discretisePathSegs
                              1000
                              ( const . metricFromLength
                                               $ (rBound-lBound)/fromIntegral xResolution
                              , resolutionFunction gs )
                              ((id&&&f)
                               . alg (\x -> ( point lBound?<x?<point rBound ?-> x )))
                trace (p:q:ps) = simpleLine p q <> trace (q:ps)
                trace _ = mempty
                gatherSides = uncurry (++) . (take 50 *** take 50)
         
         convℝ² = Dia.p2
         c = realToFrac

instance Plottable (R-->(R,R)) where
  plot f = def { relevantRange_y = mempty
               , isTintableMonochromic = True
               , axesNecessity = 1
               , dynamicPlot = plot }
   where plot gs@(GraphWindowSpecR2{..}) = curves `deepseq`
                                          mkPlot (foldMap trace curves)
          where curves :: [[P2]]
                curves = map (map $ convℝ² . snd) . gatherSides
                        $ discretisePathSegs
                              1000
                              ( const . metricFromLength $ 1/100
                              , resolutionFunction gs )
                              f
                trace (p:q:ps) = simpleLine p q <> trace (q:ps)
                trace _ = mempty
                gatherSides = uncurry (++) . (take 50 *** take 50)
         
         convℝ² = Dia.p2
         c = realToFrac


resolutionFunction :: GraphWindowSpecR2 -> RieMetric ℝ²
resolutionFunction GraphWindowSpecR2{..} = resoFunc
 where w = rBound - lBound; h = tBound - bBound
       ε = projector (recip δx, 0) ^+^ projector (0, recip δy)
       δx = w / fromIntegral xResolution
       δy = h / fromIntegral yResolution
       resoFunc (x,y)
         | x >= lBound, x <= rBound, y >= bBound, y <= tBound  = ε
         | otherwise = projector (recip qx,0) ^+^ projector (0,recip qy)
        where qx | x < lBound  = lBound - x
                 | x > rBound  = x - rBound
                 | otherwise   = δx * qy/δy
              qy | y < bBound  = bBound - y
                 | y > tBound  = y - tBound
                 | otherwise   = δy * qx/δx


instance Plottable (R-.^>R) where
  plot rPCM@(RecursivePCM gPFit gDetails gFitDevs (PCMRange x₀ wsp) gSplN ())
            = def {
                relevantRange_x = atLeastInterval $ Interval x₀ xr
              , relevantRange_y = otherDimDependence $ rPCMLinFitRange rPCM
              , isTintableMonochromic = True
              , axesNecessity = 1
              , dynamicPlot = plot
              }
   where 
         xr = wsp * fromIntegral gSplN
         plot (GraphWindowSpecR2{..}) = mkPlot . trace $ flattenPCM_resoCut bb δx rPCM
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
                 where [σp,σq] = map (`metric'`1) [σp', σq']
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
            = def {
                relevantRange_x = atLeastInterval xRange
              , relevantRange_y = atLeastInterval yRange
              , isTintableMonochromic = True
              , axesNecessity = 1
              , dynamicPlot = plot
              }
   where plot (GraphWindowSpecR2{..}) = mkPlot
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
                        where d = metriScale' σ $ turnLeft v
                trStRange (Right pts) = (`foldMap`pts)
                   $ \(p, DevBoxes dv _)
                              -> let δxm = metric' dv $ 1^&0
                                     δym = metric' dv $ 0^&1
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

-- | Simply connect the points by straight line segments, in the given order.
--   Beware that this will always slow down the performance when the list is large;
--   there is no &#201c;statistic optimisation&#201d; as in 'tracePlot'.
lineSegPlot :: [(Double, Double)] -> DynamicPlottable
lineSegPlot ps'
    | null ps        = mempty { isTintableMonochromic = True }
    | otherwise      = def {
               relevantRange_x = atLeastInterval' $ foldMap (pure . spInterval . fst) (concat ps)
             , relevantRange_y = atLeastInterval' $ foldMap (pure . spInterval . snd) (concat ps)
             , isTintableMonochromic = True
             , axesNecessity = 1
             , dynamicPlot = plot }
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

flattenPCM_P2_resoCut :: R2Box -> [DualSpace R2]
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
          | metrics' dev δs > 0.5 || (sum $ ((^2).(pa<.>^)) <$> δs) > 3
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
  plot shade = def {
                relevantRange_x = atLeastInterval xRange
              , relevantRange_y = atLeastInterval yRange
              , isTintableMonochromic = True
              , axesNecessity = 1
              , dynamicPlot = plot
              }
   where plot _ = mkPlot $ foldMap axLine eigVs 
          where axLine eigV = simpleLine (ctr .-~^ eigV) (ctr .+~^ eigV)
         (xRange,yRange) = shadeExtends shade
         ctr = shade^.shadeCtr
         eigVs = eigenSpan $ shade^.shadeExpanse

instance Plottable (Shade (R,R)) where
  plot sh = plot (coerceShade sh :: Shade P2)

instance Plottable (Shade' (R,R)) where
  plot shade = def {
                isTintableMonochromic = True
              , axesNecessity = 1
              , dynamicPlot = plot
              }
   where plot _ = mkPlot $ Dia.circle 1
                            & Dia.scaleX w₁ & Dia.scaleY w₂
                            & Dia.rotate ϑ
                            & Dia.opacity 0.2
                            & Dia.moveTo ctr
         ctr = Dia.p2 $ shade^.shadeCtr
         [ev₁@(e₁x,e₁y),ev₂] = eigenSpan' $ shade^.shadeNarrowness
         ϑ = atan2 e₁y e₁x  Dia.@@ Dia.rad
         w₁ = recip $ magnitude ev₁; w₂ = recip $ magnitude ev₂

instance Plottable (ConvexSet (R,R)) where
  plot EmptyConvex = mempty
  plot (ConvexSet hull intersects)
      = plot [ plot intersects
                 & tweakDiagram (Dia.opacity
                                     (1 / fromIntegral (length intersects)) )
             , plot hull
                 & tweakDiagram ( Dia.lwO 3
                              >>> Dia.opacity 1
                              >>> Dia.fcA (Dia.withOpacity Dia.grey 0.01) ) ]
         
instance Plottable (Shade' P2) where
  plot sh = plot (coerceShade sh :: Shade' (R,R))


instance Plottable (Shaded ℝ ℝ) where
  plot tr | length trivs' >= 2
          = def { relevantRange_x = atLeastInterval $ Interval xmin xmax
                , relevantRange_y = atLeastInterval $ Interval ymin ymax
                , isTintableMonochromic = True
                , axesNecessity = 1
                , dynamicPlot = plot
                }
   where plot grWS@(GraphWindowSpecR2{..}) = mkPlot $
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
            = [minimum, maximum]<*>[_topological<$>allLeaves, _untopological<$>allLeaves]
         lLoop ps@(p:_) = Dia.fromVertices $ ps++[p]
         leafPoints = sortBy (comparing (^._x))
                         $ (\(x`WithAny`y) -> y^&x) <$> allLeaves
         allLeaves = onlyLeaves tr
  plot _ = def

instance Plottable (PointsWeb ℝ (Shade' ℝ)) where
  plot web | length locals >= 2
          = def { relevantRange_x = atLeastInterval $ Interval xmin xmax
                , relevantRange_y = atLeastInterval $ Interval ymin ymax
                , isTintableMonochromic = True
                , axesNecessity = 1
                , dynamicPlot = plot
                }
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
          where mkTriv ((xc,Shade' yc yce), [(δxo, Shade' yo _)])
                       = case tryMetricAsLength yce of
                           Option (Just ry) ->
                              ( [ ( (xc, dirSort 0 δxo)
                                  , ( (yc, ry)
                                    , denseLinear $ \δx -> δx * (yo-yc)/δxo ) ) ], [] )
                           Option Nothing ->
                              ( [], [(xc, dirSort 0 δxo)] )
                mkTriv ((xc,Shade' yc yce), [(δxl, Shade' yl _), (δxr, Shade' yr _)])
                       = case tryMetricAsLength yce of
                           Option (Just ry) ->
                              ( [ ( (xc, dirSort δxl δxr)
                                  , ( (yc, ry)
                                    , denseLinear $ \δx -> δx * η ) ) ], [] )
                           Option Nothing ->
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

instance Plottable (SimpleTree P2) where
  plot (GenericTree Nothing) = plot ([] :: [SimpleTree P2])
  plot (GenericTree (Just (ctr, root)))
           = def{
                relevantRange_x = atLeastInterval xRange
              , relevantRange_y = atLeastInterval yRange
              , isTintableMonochromic = True
              , axesNecessity = 1
              , dynamicPlot = plot
              }
   where plot _ = mkPlot $ go 4 ctr (treeBranches root)
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


type GraphWindowSpec = GraphWindowSpecR2

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







data Plot = Plot {
       plotAnnotations :: [Annotation]
     , getPlot :: PlainGraphicsR2
  }
instance Semigroup Plot where
  Plot a1 d1 <> Plot a2 d2 = Plot (a1<>a2) (d1<>d2)
instance Monoid Plot where
  mempty = Plot mempty mempty
  mappend = (<>)

mkPlot :: PlainGraphicsR2 -> Plot
mkPlot = Plot mempty

mkAnnotatedPlot :: [Annotation] -> PlainGraphicsR2 -> Plot
mkAnnotatedPlot ans = Plot ans

data DynamicPlottable = DynamicPlottable { 
        relevantRange_x, relevantRange_y :: RangeRequest R
      , isTintableMonochromic :: Bool
      , occlusiveness :: Double
         -- ^ How surface-occupying the plot is.
         --   Use positive values for opaque 2D plots that would tend to obscure
         --   other objects, negative values for sparse/small point plots.
         --   The z-order will be chosen accordingly.
      , axesNecessity :: Necessity
      , legendEntries :: [LegendEntry]
      , dynamicPlot :: GraphWindowSpec -> Plot
  }

instance Semigroup DynamicPlottable where
  DynamicPlottable rx₁ ry₁ tm₁ oc₁ ax₁ le₁ dp₁
    <> DynamicPlottable rx₂ ry₂ tm₂ oc₂ ax₂ le₂ dp₂
        = DynamicPlottable
   (rx₁<>rx₂) (ry₁<>ry₂) (tm₁||tm₂) (oc₁+oc₂) (ax₁+ax₂) (le₁++le₂) (dp₁<>dp₂) 
instance Monoid DynamicPlottable where
  mempty = DynamicPlottable mempty mempty False 0 0 [] mempty
  mappend = (<>)
instance Default DynamicPlottable where def = mempty

data GraphViewState = GraphViewState {
        lastStableView :: Maybe (GraphWindowSpec, Plot)
      , realtimeView, nextTgtView :: Async Plot
      , graphColor :: Maybe AColour
   }



legendName :: String -> DynamicPlottable -> DynamicPlottable
legendName n d = d { legendEntries = LegendEntry (PlainText n) mempty : legendEntries d }


data RangeRequest r 
       = OtherDimDependantRange (Option (Interval r) -> Option (Interval r))
       | MustBeThisRange (Interval r)

instance (Ord r) => Semigroup (RangeRequest r) where
  MustBeThisRange r <> _ = MustBeThisRange r
  _ <> MustBeThisRange r = MustBeThisRange r
  OtherDimDependantRange r1 <> OtherDimDependantRange r2 = OtherDimDependantRange $ r1<>r2
instance (Ord r) => Monoid (RangeRequest r) where
  mempty = OtherDimDependantRange $ const mempty
  mappend = (<>)

otherDimDependence :: (Interval r->Interval r) -> RangeRequest r
otherDimDependence = OtherDimDependantRange . fmap

atLeastInterval :: Interval r -> RangeRequest r
atLeastInterval = atLeastInterval' . pure

atLeastInterval' :: Option (Interval r) -> RangeRequest r
atLeastInterval' = OtherDimDependantRange . const




                

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
--   <<images/examples/cos-encircle-points.gif>>
--  
--   The individual objects you want to plot can be evaluated in multiple threads, so
--   a single hard calculatation won't freeze the responsitivity of the whole window.
--   Invoke e.g. from @ghci +RTS -N4@ to benefit from this.
--   
--   ATTENTION: the window may sometimes freeze, especially when displaying 
--   complicated functions with 'fnPlot` from ghci. This is apparently
--   a kind of deadlock problem with one of the C libraries that are invoked,
--   in particular, 'fnPlot' makes heavy use of <http://hackage.haskell.org/package/hmatrix hmatrix>
--   and thus <http://www.gnu.org/software/gsl/ GSL>.
--   At the moment, we can recommend no better solution than to abort and restart ghci
--   (or what else you use – iHaskell kernel, process, ...) if this occurs.
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
               graphs'' = sortBy (comparing occlusiveness) graphs'
           w <- mapM newIORef $ replicate 2 window₀
           gs <- newIORef =<< assignGrViews graphs'' defaultColourSeq 0
           return (w,gs)
   
   
   GTK.initGUI
   window <- GTK.windowNew
   
   mouseAnchor <- newIORef Nothing
                 
   refreshDraw <- do
       drawA <- GTK.drawingAreaNew
       GTK.onExpose drawA $ \_ -> do
                (canvasX,canvasY) <- GTK.widgetGetSize drawA
                modifyIORef viewTgt $ \view -> view{ xResolution = fromIntegral canvasX
                                                   , yResolution = fromIntegral canvasY }
                dia <- readIORef dgStore
                let oldSize = Dia.size dia
                    scaledDia = Dia.bg Dia.black
                                . Dia.scaleX (fromInt canvasX / 2)
                                . Dia.scaleY (-fromInt canvasY / 2)
                                . Dia.translate (1 ^& (-1))
                                . Dia.withEnvelope (Dia.rect 2 2 :: PlainGraphicsR2)
                                  $ dia
                drawWindow <- GTK.widgetGetDrawWindow drawA
                BGTK.renderToGtk drawWindow $ scaledDia
                return True
       
       GTK.on drawA GTK.buttonPressEvent . Event.tryEvent $ do
            Event.eventButton >>= guard.(==defaultDragButton)
            anchXY <- Event.eventCoordinates
            liftIO . writeIORef mouseAnchor $ Just anchXY
       GTK.on drawA GTK.buttonReleaseEvent . Event.tryEvent $ do
            Event.eventButton >>= guard.(==defaultDragButton)
            liftIO . writeIORef mouseAnchor $ Nothing
       
       GTK.on drawA GTK.motionNotifyEvent . Event.tryEvent $ do
          liftIO (readIORef mouseAnchor) >>= \case
             Just (oldX,oldY) -> do
                (mvX,mvY) <- Event.eventCoordinates
                (canvasX,canvasY) <- liftIO $ GTK.widgetGetSize drawA
                let ηX = (oldX-mvX) / fromIntegral canvasX
                    ηY = (mvY-oldY) / fromIntegral canvasY
                liftIO . modifyIORef viewTgt $ \view@GraphWindowSpecR2{..} ->
                    let w = rBound - lBound
                        h = tBound - bBound
                    in view{ lBound = lBound + w * ηX
                           , rBound = rBound + w * ηX
                           , tBound = tBound + h * ηY
                           , bBound = bBound + h * ηY
                           }
                liftIO . modifyIORef mouseAnchor . fmap $ const (mvX,mvY)
             Nothing -> mzero
       GTK.widgetAddEvents drawA [GTK.ButtonMotionMask]
       
       GTK.on drawA GTK.scrollEvent . Event.tryEvent $ do
                (canvasX,canvasY) <- liftIO $ GTK.widgetGetSize drawA
                (scrollX,scrollY) <- Event.eventCoordinates
                let (rcX,rcY) = ( scrollX*2 / fromIntegral canvasX - 1
                                , 1 - scrollY*2 / fromIntegral canvasY )
                scrollD <- Event.eventScrollDirection
                liftIO . modifyIORef viewTgt $ \view@GraphWindowSpecR2{..} ->
                  let w = rBound - lBound
                      h = tBound - bBound
                      ηl = (rcX + 1)^2/4; ηr = (rcX - 1)^2/4
                      ηb = (rcY + 1)^2/4; ηt = (rcY - 1)^2/4
                      ηh = (1-ηt) * (1-ηb) + ηl + ηr
                      ηv = (1-ηl) * (1-ηr) + ηt + ηb
                   in case defaultScrollBehaviour scrollD of
                        ScrollZoomIn -> view{
                            lBound = lBound + w * ηl * ηh * scrollZoomStrength
                          , rBound = rBound - w * ηr * ηh * scrollZoomStrength
                          , tBound = tBound - h * ηt * ηv * scrollZoomStrength
                          , bBound = bBound + h * ηb * ηv * scrollZoomStrength
                          }
                        ScrollZoomOut -> view{
                            lBound = lBound - w * ηr * ηh * scrollZoomStrength
                          , rBound = rBound + w * ηl * ηh * scrollZoomStrength
                          , tBound = tBound + h * ηb * ηv * scrollZoomStrength
                          , bBound = bBound - h * ηt * ηv * scrollZoomStrength
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
   
   
   let refreshScreen = do
           currentView@(GraphWindowSpecR2{..}) <- readIORef viewState
           let normaliseView :: PlainGraphicsR2 -> PlainGraphicsR2
               normaliseView = (Dia.scaleX xUnZ :: PlainGraphicsR2->PlainGraphicsR2)
                                  . Dia.scaleY yUnZ
                                . Dia.translate (Dia.r2(-x₀,-y₀))
                  where xUnZ = 1/w; yUnZ = 1/h
               w = (rBound - lBound)/2; h = (tBound - bBound)/2
               x₀ = lBound + w; y₀ = bBound + h
               textTK txSiz asp = TextTK defaultTxtStyle txSiz asp 0.2 0.2
               renderComp (DynamicPlottable{..}, GraphViewState{..}) = do
                   plt <- poll realtimeView >>= \case
                                  Just (Right pl) -> return $ Just pl
                                  _ -> case lastStableView of
                                   Just (_, vw) -> return $ Just vw
                                   _ -> poll nextTgtView >> return Nothing
                   case plt of
                    Nothing -> return mempty
                    Just Plot{..} -> let 
                       antTK = DiagramTK { viewScope = currentView 
                                         , textTools = textTK txtSize aspect }
                       txtSize = h * fontPts / fromIntegral yResolution
                       aspect  = w * fromIntegral yResolution
                                                         / (h * fromIntegral xResolution)
                       fontPts = 12
                       transform :: PlainGraphicsR2 -> PlainGraphicsR2
                       transform = normaliseView . clr
                         where clr | Just c <- graphColor  = Dia.lcA c . Dia.fcA c
                                   | otherwise             = id
                     in do
                       renderedAnnot <- mapM (prerenderAnnotation antTK) plotAnnotations
                       return . transform $ fold renderedAnnot <> getPlot

           gvStates <- readIORef graphs
           waitAny $ map (realtimeView . snd) gvStates
           
           thePlot <- (mconcat . reverse) <$> mapM renderComp (reverse gvStates)
           theLegend <- prerenderLegend (textTK 10 1) colourScheme
                $ (\(p,g) -> (,) <$> legendEntries p <*> [graphColor g]) =<< gvStates
                   
           writeIORef dgStore $ ( theLegend & Dia.scaleX (0.1 / sqrt (fromIntegral xResolution))
                                            & Dia.scaleY (0.1 / sqrt (fromIntegral yResolution)) 
                                            & (`Dia.place`(0.75^&0.75)) )
                                <> thePlot
                                                    
           refreshDraw
           
   let mainLoop = do
           t <- getCurrentTime
           δt <- fmap (diffUTCTime t) $ readIORef lastFrameTime
           writeIORef lastFrameTime t
   
           do vt <- readIORef viewTgt
              updateRTView $ \vo -> 
                   let a%b = let η = min 1 $ 2 * realToFrac δt in η*a + (1-η)*b 
                   in GraphWindowSpecR2 (lBound vt % lBound vo) (rBound vt % rBound vo)
                                        (bBound vt % bBound vo) (tBound vt % tBound vo)
                                        (xResolution vt) (yResolution vt)
                                        defColourScheme
           -- GTK.sleep 0.01
           refreshScreen
           -- GTK.pollEvents
           return True
   
   GTK.onDestroy window $ do
        (readIORef graphs >>=) . mapM_  -- cancel remaining threads
           $ \(_, GraphViewState{..}) -> cancel realtimeView >> cancel nextTgtView
        GTK.mainQuit
                 
   
   GTK.timeoutAdd mainLoop 100
   

   GTK.mainGUI
   
   readIORef viewState


autoDefaultView :: [DynamicPlottable] -> GraphWindowSpec
autoDefaultView graphs = GraphWindowSpecR2 l r b t defResX defResY defaultColourScheme
  where (xRange, yRange) = foldMap (relevantRange_x &&& relevantRange_y) graphs
        ((l,r), (b,t)) = ( xRange `dependentOn` yRange
                         , yRange `dependentOn` xRange )
        
        dependentOn :: RangeRequest R -> RangeRequest R -> (R,R)
        MustBeThisRange (Interval a b) `dependentOn` _ = (a,b)
        OtherDimDependantRange ξ `dependentOn` MustBeThisRange i
           = addMargin . defRng . ξ $ pure i
        OtherDimDependantRange ξ `dependentOn` OtherDimDependantRange υ
           = addMargin . defRng . ξ . pure . defRng $ υ mempty
        
        defRng (Option (Just (Interval a b))) | b>a     
                  = Interval a b
        defRng _  = Interval (-1) 1   -- ad-hoc hack to catch NaNs etc..
        addMargin (Interval a b) = (a - q, b + q)
            where q = (b - a) / 6
  


defResX, defResY :: Integral i => i
defResX = 640
defResY = 480


data ScrollAction = ScrollZoomIn | ScrollZoomOut

defaultScrollBehaviour :: Event.ScrollDirection -> ScrollAction
defaultScrollBehaviour Event.ScrollUp = ScrollZoomIn
defaultScrollBehaviour Event.ScrollDown = ScrollZoomOut

defaultDragButton :: Event.MouseButton
defaultDragButton = Event.MiddleButton

scrollZoomStrength :: Double
scrollZoomStrength = 1/20





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
continFnPlot f = def{
               relevantRange_y = otherDimDependence yRangef
             , isTintableMonochromic = True
             , axesNecessity = 1
             , dynamicPlot = plot }
 where yRangef = onInterval $ \(l, r) -> ((!10) &&& (!70)) . sort . pruneOutlyers
                                               $ map f [l, l + (r-l)/80 .. r]
       plot (GraphWindowSpecR2{..}) = curve `deepseq` mkPlot (trace curve)
        where δx = (rBound - lBound) * 2 / fromIntegral xResolution
              curve = [ (x ^& f x) | x<-[lBound, lBound+δx .. rBound] ]
              trace (p:q:ps) = simpleLine p q <> trace (q:ps)
              trace _ = mempty
       pruneOutlyers = filter (not . isNaN) 
       l!n | (x:_)<-drop n l  = x
           | otherwise         = error "Function appears to yield NaN most of the time. Cannot be plotted."


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
fnPlot :: (∀ m . ( WithField ℝ PseudoAffine m
                         , HasMetric (Needle (Interior m)) )
                         => AgentVal (-->) m ℝ -> AgentVal (-->) m ℝ )
                     -> DynamicPlottable
fnPlot f = plot fd
 where fd :: ℝ --> ℝ
       fd = alg f

-- | Plot a continuous, “parametric function”, i.e. mapping the real line to a path in ℝ².
paramPlot :: (∀ m . ( WithField ℝ PseudoAffine m
                         , HasMetric (Needle (Interior m)) )
                       => AgentVal (-->) m ℝ -> (AgentVal (-->) m ℝ, AgentVal (-->) m ℝ) )
                     -> DynamicPlottable
paramPlot f = plot fd
 where fd :: ℝ --> (ℝ,ℝ)
       fd = alg1to2 f

scrutiniseDiffability :: (∀ m . ( WithField ℝ PseudoAffine m
                                , HasMetric (Needle (Interior m)) )
                         => AgentVal (-->) m ℝ -> AgentVal (-->) m ℝ )
                     -> DynamicPlottable
scrutiniseDiffability f = plot [{-plot fd, -}dframe 0.2, dframe 0.02]
 where fd :: ℝ --> ℝ
       fd = alg f
       fscrut = analyseLocalBehaviour fd
       dframe rfh = def{
                   isTintableMonochromic = True
                 , dynamicPlot = mkFrame
                 }
        where mkFrame (GraphWindowSpecR2{..}) = case fscrut xm of
                      Option (Just ((ym,y'm), δOδx²))
                        | Option (Just δx) <- δOδx² δy
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
continColourSurfaceFnPlot f = def {
               axesNecessity = 1
             , occlusiveness = 4
             , dynamicPlot = plot }
 where plot (GraphWindowSpecR2{..}) = mkPlot
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
dynamicAxes = def { 
               axesNecessity = superfluent
             , occlusiveness = 1
             , dynamicPlot = plot }
 where plot gwSpec@(GraphWindowSpecR2{..}) = Plot labels lines
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


noDynamicAxes :: DynamicPlottable
noDynamicAxes = def { axesNecessity = superfluent }



simpleLine :: P2 -> P2 -> PlainGraphicsR2
simpleLine = simpleLine' 2

simpleLine' :: Double -> P2 -> P2 -> PlainGraphicsR2
simpleLine' w p q = Dia.fromVertices [p,q] & Dia.lwO w

autoDashLine :: Double -> P2 -> P2 -> PlainGraphicsR2
autoDashLine w p q = simpleLine' (max 1 w) p q
       & if w < 1 then Dia.dashingO [w*6, 3] 0 else id


tweakDiagram :: (PlainGraphicsR2->PlainGraphicsR2) -> DynamicPlottable->DynamicPlottable
tweakDiagram f fig = fig { dynamicPlot = tweak . dynamicPlot fig }
 where tweak pl = pl { getPlot = f $ getPlot pl }

opacityFactor :: Double -> DynamicPlottable -> DynamicPlottable
opacityFactor = tweakDiagram . Dia.opacity


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

xInterval (l,r) = mempty { relevantRange_x = atLeastInterval $ Interval l r }
forceXRange (l,r) = mempty { relevantRange_x = MustBeThisRange $ Interval l r }
yInterval (b,t) = mempty { relevantRange_y = atLeastInterval $ Interval b t }
forceYRange (b,t) = mempty { relevantRange_y = MustBeThisRange $ Interval b t }
 




-- | 'ViewXCenter', 'ViewYResolution' etc. can be used as arguments to some object
--   you 'plot', if its rendering is to depend explicitly on the screen's visible range.
--   You should not need to do that manually except for special applications (the
--   standard plot objects like 'fnPlot' already take the range into account anyway)
--   &#x2013; e.g. comparing  with the linear regression /of all visible points/
--   from some sample with some function's tangent /at the screen center/.
--   
-- @
-- plotWindow [fnPlot sin, plot $ \\(ViewXCenter xc) x -> sin xc + (x-xc) * cos xc]
-- @
-- 
--   <<images/examples/sin-ctrd-tangents.gif>>
newtype ViewXCenter = ViewXCenter { getViewXCenter :: Double }
instance (Plottable p) => Plottable (ViewXCenter -> p) where
  plot f = def { relevantRange_y = OtherDimDependantRange $
                     \g -> deescalate relevantRange_y g . plot . f . cxI =<< g
               , isTintableMonochromic = isTintableMonochromic fcxVoid
               , axesNecessity = axesNecessity fcxVoid
               , dynamicPlot = \g -> dynamicPlot (plot . f $ cx g) g }
    where cx (GraphWindowSpecR2{..}) = ViewXCenter $ (lBound+rBound)/2
          cxI (Interval l r) = ViewXCenter $ (l+r)/2
          fcxVoid = plot . f $ ViewXCenter 0.23421  -- Yup, it's magic.
          deescalate rfind otherdim p = case rfind p of
             MustBeThisRange i -> pure i
             OtherDimDependantRange ifr -> ifr otherdim
newtype ViewYCenter = ViewYCenter { getViewYCenter :: Double }
instance (Plottable p) => Plottable (ViewYCenter -> p) where
  plot f = def { relevantRange_x = OtherDimDependantRange $
                     \g -> deescalate relevantRange_x g . plot . f . cyI =<< g
               , isTintableMonochromic = isTintableMonochromic fcyVoid
               , axesNecessity = axesNecessity fcyVoid
               , dynamicPlot = \g -> dynamicPlot (plot . f $ cy g) g }
    where cy (GraphWindowSpecR2{..}) = ViewYCenter $ (bBound+tBound)/2
          cyI (Interval b t) = ViewYCenter $ (b+t)/2
          fcyVoid = plot . f $ ViewYCenter 0.319421  -- Alright, alright... the idea is to avoid exact equality with zero or any other number that might come up in some plot object, since such an equality can lead to div-by-zero problems.
          deescalate rfind otherdim p = case rfind p of
             MustBeThisRange i -> pure i
             OtherDimDependantRange ifr -> ifr otherdim
newtype ViewWidth = ViewWidth { getViewWidth :: Double }
instance (Plottable p) => Plottable (ViewWidth -> p) where
  plot f = def { relevantRange_y = OtherDimDependantRange $
                     \g -> deescalate relevantRange_y g . plot . f . wI =<< g
               , isTintableMonochromic = isTintableMonochromic fwVoid
               , axesNecessity = axesNecessity fwVoid
               , dynamicPlot = \g -> dynamicPlot (plot . f $ w g) g }
    where w (GraphWindowSpecR2{..}) = ViewWidth $ rBound - lBound
          wI (Interval l r) = ViewWidth $ r - l
          fwVoid = plot . f $ ViewWidth 2.142349
          deescalate rfind otherdim p = case rfind p of
             MustBeThisRange i -> pure i
             OtherDimDependantRange ifr -> ifr otherdim
newtype ViewHeight = ViewHeight { getViewHeight :: Double }
instance (Plottable p) => Plottable (ViewHeight -> p) where
  plot f = def { relevantRange_x = OtherDimDependantRange $
                     \g -> deescalate relevantRange_x g . plot . f . hI =<< g
               , isTintableMonochromic = isTintableMonochromic fhVoid
               , axesNecessity = axesNecessity fhVoid
               , dynamicPlot = \g -> dynamicPlot (plot . f $ h g) g }
    where h (GraphWindowSpecR2{..}) = ViewHeight $ tBound - bBound
          hI (Interval b t) = ViewHeight $ t - b
          fhVoid = plot . f $ ViewHeight 1.494213
          deescalate rfind otherdim p = case rfind p of
             MustBeThisRange i -> pure i
             OtherDimDependantRange ifr -> ifr otherdim
newtype ViewXResolution = ViewXResolution { getViewXResolution :: Int }
newtype ViewYResolution = ViewYResolution { getViewYResolution :: Int }




atExtendOf :: PlainGraphicsR2 -> PlainGraphicsR2 -> PlainGraphicsR2
atExtendOf d₁ = atExtendOf' d₁ 1

atExtendOf' :: PlainGraphicsR2 -> Double -> PlainGraphicsR2 -> PlainGraphicsR2
atExtendOf' d₁ q d₂ = d₂
                      & Dia.translate ((pux+plx-lux-llx)/2 ^& (puy+ply-luy-lly)/2)
                      & Dia.scaleX (q*(pux-plx)/(lux-llx))
                      & Dia.scaleY (q*(puy-ply)/(luy-lly))
 where (Just (plx,pux)) = Dia.extentX d₁; (Just (ply,puy)) = Dia.extentY d₁
       (Just (llx,lux)) = Dia.extentX d₂; (Just (lly,luy)) = Dia.extentY d₂



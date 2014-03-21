-- |
-- Module      : Graphics.Dynamic.Plot.R2
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}

module Graphics.Dynamic.Plot.R2 (
        -- * Interactive display
          plotWindow
        -- * Plottable objects
        -- ** Class  
        , Plottable(..)
        -- ** Simple function plots 
        , fnPlot, continFnPlot, continParamPlot
        -- ** View selection
        , xInterval, yInterval
        ) where

import Graphics.Dynamic.Plot.Colour

import Graphics.DrawingCombinators ((%%), R, R2)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as OpenGL
import Graphics.Rendering.OpenGL (($=))

import Control.Category.Constrained.Prelude
import Control.Arrow.Constrained
import Control.Monad.Constrained

  
import Control.Concurrent.Async
import Control.DeepSeq


import Data.List (foldl', sort, intercalate, isPrefixOf, isInfixOf, find, zip4)
import Data.Maybe
import Data.Semigroup
import Data.Foldable (foldMap)
import Data.Function (on)
import qualified Data.Map.Lazy as Map

import Data.Manifold ((:-->))
import qualified Data.Manifold as 𝓒⁰
  
import Text.Printf

import Data.IORef

import System.IO
import System.Exit
import System.Process
import Data.Time






class Plottable p where
  plot :: p -> DynamicPlottable

instance (RealFloat r₁, RealFloat r₂) => Plottable (r₁ -> r₂) where
  plot f = fnPlot $ realToFrac . f . realToFrac

{-# RULES "plot/R->R" plot = fnPlot #-}

instance Plottable (Double :--> Double) where
  plot f = DynamicPlottable{
                       relevantRange_x = const mempty
                     , relevantRange_y = fmap . onInterval $ convℝ² . yRangef . convℝ²
                     -- , usesNormalisedCanvas = False
                     , isTintableMonochromic = True
                     , axesNecessity = 1
                     , dynamicPlot = plot }
   where yRangef (l, r) = (minimum &&& maximum) 
                            . map snd $ 𝓒⁰.finiteGraphContinℝtoℝ
                                         (𝓒⁰.GraphWindowSpec l r fgb fgt 9 9) f
          where (fgb, fgt) = (minimum &&& maximum) [f $ l, f $ m, f $ r]
                m = l + (r-l) * 0.352479608143
         
         plot (GraphWindowSpec{..}) = curve `deepseq` Plot (trace curve) []
          where curve :: [(R, R)]
                curve = map convℝ² $ 𝓒⁰.finiteGraphContinℝtoℝ mWindow f
                mWindow = 𝓒⁰.GraphWindowSpec (c lBound) (c rBound) (c bBound) (c tBound) 
                                                 xResolution yResolution
                trace (p:q:ps) = Draw.line p q <> trace (q:ps)
                trace _ = mempty
         
         convℝ² = c *** c
         c = realToFrac

instance Plottable (Double :--> (Double, Double)) where
  plot f = DynamicPlottable{
                       relevantRange_x = const mempty
                     , relevantRange_y = const mempty
                     -- , usesNormalisedCanvas = False
                     , isTintableMonochromic = True
                     , axesNecessity = 1
                     , dynamicPlot = plot }
   where plot (GraphWindowSpec{..}) = curves `deepseq` Plot (foldMap trace curves) []
          where curves :: [[(R, R)]]
                curves = map (map convℝ²) $ 𝓒⁰.finiteGraphContinℝtoℝ² mWindow f
                mWindow = 𝓒⁰.GraphWindowSpec (c lBound) (c rBound) (c bBound) (c tBound) 
                                                 xResolution yResolution
                trace (p:q:ps) = Draw.line p q <> trace (q:ps)
                trace _ = mempty
         
         convℝ² = c *** c
         c = realToFrac
 





data GraphWindowSpec = GraphWindowSpec {
      lBound, rBound, bBound, tBound :: R
    , xResolution, yResolution :: Int
  } deriving (Show)

moveStepRel :: (R, R)  -- ^ Relative translation @(Δx/w, Δy/h)@.
            -> (R, R)  -- ^ Relative zoom.
            -> GraphWindowSpec -> GraphWindowSpec
moveStepRel (δx,δy) (ζx,ζy) (GraphWindowSpec l r b t xRes yRes)
  = GraphWindowSpec l' r' b' t' xRes yRes
 where qx = (r-l)/2                  ; qy = (t-b)/2
       mx'= l + qx*(1+δx)            ; my'= b + qy*(1+δy) 
       qx'= zoomSafeGuard mx' $ qx/ζx; qy'= zoomSafeGuard my' $ qy/ζy
       l' = mx' - qx'                ; b' = my' - qy'
       r' = mx' + qx'                ; t' = my' + qy'
       zoomSafeGuard m = max (1e-250 + abs m*1e-6) . min 1e+250



data Interval = Interval !R !R
instance Semigroup Interval where  -- WRT closed hull of the union.
  Interval l₁ u₁ <> Interval l₂ u₂ = Interval (min l₁ l₂) (max u₁ u₂)
onInterval :: ((R,R) -> (R,R)) -> Interval -> Interval 
onInterval f (Interval l r) = uncurry Interval $ f (l, r)

data Plot = Plot {
       getPlot :: Draw.Image Any
     , plotAnnotations :: [Annotation]
  }

data DynamicPlottable = DynamicPlottable { 
        relevantRange_x, relevantRange_y :: Option Interval -> Option Interval
      -- , usesNormalisedCanvas :: Bool
      , isTintableMonochromic :: Bool
      , axesNecessity :: Double
      , dynamicPlot :: GraphWindowSpec -> Plot
  }

data GraphViewState = GraphViewState {
        lastStableView :: Maybe (GraphWindowSpec, Plot)
      , realtimeView, nextTgtView :: Async Plot
      , graphColor :: Maybe Draw.Color
   }


initScreen :: IO ()
initScreen = do
    True <- GLFW.initialize
    True <- GLFW.openWindow (OpenGL.Size defResX defResY) [] GLFW.Window
    GLFW.windowTitle $= "Plot"
    GLFW.swapInterval $= 1
    return ()
                

plotWindow :: [DynamicPlottable] -> IO GraphWindowSpec
plotWindow graphs' = do
   
   initScreen
   
   defaultFont <- loadFont
   
   
   ([viewTgt, viewState], graphs) <- do
           let window₀ = autoDefaultView graphs'
               assignGrViews (g@DynamicPlottable{..}:gs) (c:cs) axn = do 
                   v <- async $ return $! dynamicPlot window₀
                   fmap ((g, GraphViewState { lastStableView = Nothing
                                            , realtimeView = v, nextTgtView = v 
                                            , graphColor = cl }
                        ) : ) $ assignGrViews gs cs' (axn + axesNecessity)
                where (cl, cs')
                        | isTintableMonochromic  = (Just $ defaultColourScheme c, cs)
                        | otherwise              = (Nothing, c:cs)
               assignGrViews [] _ axesNeed 
                 | axesNeed > 0  = assignGrViews [dynamicAxes] [grey] (-1)
                 | otherwise     = return []
           w <- mapM newIORef $ replicate 2 window₀
           gs <- newIORef =<< assignGrViews graphs' defaultColourSeq 0
           return (w,gs)
   
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
   
   done      <- newIORef False
   
   let refreshScreen = do
           currentView@(GraphWindowSpec{..}) <- readIORef viewState
           let normaliseView = (Draw.scale xUnZ yUnZ <> Draw.translate (-x₀,-y₀) %%)
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
                                         , textTools = TextTK defaultFont txtSize aspect 0.2 0.2 }
                       txtSize -- | usesNormalisedCanvas  = fontPts / fromIntegral yResolution
                               | otherwise             = h * fontPts / fromIntegral yResolution
                       aspect  -- | usesNormalisedCanvas  = 1
                               | otherwise             = w * fromIntegral yResolution
                                                         / (h * fromIntegral xResolution)
                       fontPts = 12
                       transform = nmScale . clr
                         where clr | Just c <- graphColor  = Draw.tint c
                                   | otherwise             = id
                               nmScale -- | usesNormalisedCanvas  = id
                                       | otherwise             = normaliseView
                     in transform $ foldMap (prerenderAnnotation antTK) plotAnnotations <> getPlot

           gvStates <- readIORef graphs
           waitAny $ map (realtimeView . snd) gvStates
           render . mconcat . reverse =<< mapM renderComp (reverse gvStates)
           GLFW.swapBuffers
           
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
           GLFW.sleep 0.01
           refreshScreen
           GLFW.pollEvents
           readIORef done >>= \fine -> unless fine mainLoop
   
   let keyImpact key = do
           t <- getCurrentTime
           Just (_, impact) <- fmap (Map.lookup key) $ readIORef keyImpactState
           modifyIORef keyImpactState $ Map.adjust ( \(t₁, p)
                       -> (t, min 1 $ ( (p - minKeyImpact) * (exp . (*3) . realToFrac $ diffUTCTime t₁ t)
                                       + minKeyImpact ) * 2 )
                   ) key
           return impact
   
   GLFW.keyCallback $= \key state -> do
           let keyStepSize = 0.1
           (state==GLFW.Press) `when` do
              case defaultKeyMap key of
                Just QuitProgram -> writeIORef done True
                Just movement    -> do
                   impact <- keyImpact movement
                   updateTgtView $ case movement of
                    MoveUp    -> moveStepRel (0,  impact) (1, 1)
                    MoveDown  -> moveStepRel (0, -impact) (1, 1)
                    MoveLeft  -> moveStepRel (-impact, 0) (1, 1)
                    MoveRight -> moveStepRel (impact , 0) (1, 1)
                    ZoomIn_x  -> moveStepRel (0, 0)   (1+impact, 1)
                    ZoomOut_x -> moveStepRel (0, 0)   (1-impact/2, 1)
                    ZoomIn_y  -> moveStepRel (0, 0)   (1, 1+impact/2)
                    ZoomOut_y -> moveStepRel (0, 0)   (1, 1-impact/2)
                _ -> return ()
           
   GLFW.windowSizeCallback $= \s@(OpenGL.Size xRes yRes) -> do
           OpenGL.viewport $= (OpenGL.Position 0 0, s)
           modifyIORef viewTgt $ \view -> view{ xResolution = fromIntegral xRes
                                              , yResolution = fromIntegral yRes }
           -- refreshScreen
           
   GLFW.windowCloseCallback $= do
           writeIORef done True
           return True
                 
   
   -- putStrLn "Enter Main loop..."
   
   mainLoop
   
   (readIORef graphs >>=) . mapM_  -- cancel remaining threads
        $ \(_, GraphViewState{..}) -> cancel realtimeView >> cancel nextTgtView
   
   -- putStrLn "Done."
   
   GLFW.terminate
   
   readIORef viewState


autoDefaultView :: [DynamicPlottable] -> GraphWindowSpec
autoDefaultView graphs = GraphWindowSpec l r b t defResX defResY
  where (xRange, yRange) = foldMap (relevantRange_x &&& relevantRange_y) graphs
        ((l,r), (b,t)) = ( xRange `dependentOn` yRange
                         , yRange `dependentOn` xRange )
        ξ`dependentOn`υ = addMargin . defRng . ξ . return . defRng $ υ mempty
        defRng = Interval (-1) 1 `option` id
        addMargin (Interval a b) = (a - q, b + q)
            where q = (b - a) / 6
  


render :: Monoid a => Draw.Image a -> IO()
render = Draw.clearRender

defResX, defResY :: Integral i => i
defResX = 640
defResY = 480


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

defaultKeyMap :: GLFW.Key -> Maybe KeyAction
defaultKeyMap (GLFW.SpecialKey GLFW.UP   ) = Just MoveUp
defaultKeyMap (GLFW.SpecialKey GLFW.DOWN ) = Just MoveDown
defaultKeyMap (GLFW.SpecialKey GLFW.LEFT ) = Just MoveLeft
defaultKeyMap (GLFW.SpecialKey GLFW.RIGHT) = Just MoveRight
defaultKeyMap (GLFW.CharKey 'K') = Just MoveUp
defaultKeyMap (GLFW.CharKey 'J') = Just MoveDown
defaultKeyMap (GLFW.CharKey 'H') = Just MoveLeft
defaultKeyMap (GLFW.CharKey 'L') = Just MoveRight
defaultKeyMap (GLFW.CharKey 'B') = Just ZoomIn_x
defaultKeyMap (GLFW.CharKey 'N') = Just ZoomOut_x
defaultKeyMap (GLFW.CharKey 'I') = Just ZoomIn_y
defaultKeyMap (GLFW.CharKey 'O') = Just ZoomOut_y
defaultKeyMap (GLFW.SpecialKey GLFW.ESC) = Just QuitProgram
defaultKeyMap _ = Nothing

instance NFData Draw.R


fnPlot :: (R -> R) -> DynamicPlottable
fnPlot f = DynamicPlottable{
               relevantRange_x = const mempty
             , relevantRange_y = yRangef
             -- , usesNormalisedCanvas = False
             , isTintableMonochromic = True
             , axesNecessity = 1
             , dynamicPlot = plot }
 where yRangef = fmap . onInterval $ \(l, r) -> ((!10) &&& (!70)) . sort . pruneOutlyers
                                               $ map f [l, l + (r-l)/80 .. r]
       plot (GraphWindowSpec{..}) = curve `deepseq` Plot (trace curve) []
        where δx = (rBound - lBound) * 2 / fromIntegral xResolution
              curve = [ (x, f x) | x<-[lBound, lBound+δx .. rBound] ]
              trace (p:q:ps) = Draw.line p q <> trace (q:ps)
              trace _ = mempty
       pruneOutlyers = filter (not . isNaN) 
       l!n | (x:_)<-drop n l  = x
           | otherwise         = error "Function appears to yield NaN most of the time. Cannot be plotted."

continFnPlot :: (forall m . 𝓒⁰.Manifold m 
                   => ProxyVal (:-->) m Double -> ProxyVal (:-->) m Double) 
                      -> DynamicPlottable
continFnPlot f = plot fc
 where fc :: Double :--> Double
       fc = alg f
       
continParamPlot :: (forall m . 𝓒⁰.Manifold m 
                    => ProxyVal (:-->) m Double 
                        -> (ProxyVal (:-->) m Double, ProxyVal (:-->) m Double)) 
                     -> DynamicPlottable
continParamPlot f = plot fc
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
              ceil = fromIntegral . ceiling
              flor = fromIntegral . floor
       lvlSpecs = [ (80, 0.3), (18, 0.1) ]



dynamicAxes :: DynamicPlottable
dynamicAxes = DynamicPlottable { 
               relevantRange_x = const mempty
             , relevantRange_y = const mempty   
             -- , usesNormalisedCanvas = False
             , isTintableMonochromic = True
             , axesNecessity = -1
             , dynamicPlot = plot }
 where plot gwSpec@(GraphWindowSpec{..}) = Plot lines labels
        where (DynamicAxes yAxCls xAxCls) = crtDynamicAxes gwSpec
              lines = Draw.line (lBound, 0) (rBound, 0)  `provided`(bBound < 0 && tBound > 0)
                   <> Draw.line (0, bBound) (0, tBound)  `provided`(lBound < 0 && rBound > 0)
                   <> foldMap (renderClass $ \x -> ((x, bBound), ((x, tBound)))) yAxCls
                   <> foldMap (renderClass $ \y -> ((lBound, y), ((rBound, y)))) xAxCls
              labels = do (dirq, hAlign, vAlign, acl) <- zip4 [\x->(x,0), \y->(0,y) ] 
                                                              [AlignMid , AlignTop  ]
                                                              [AlignTop , AlignMid  ]
                                                              [yAxCls   , xAxCls    ]
                          let (AxisClass vaxs _ prc) = head acl
                              prepAnnotation (Axis{axisPosition=z}) = do
                                               guard(z/=0) 
                                               [Annotation (TextAnnotation txt align) place False]
                               where txt = PlainText . prettyFloatShow prc $ realToFrac z
                                     place = ExactPlace $ dirq z
                                     align = TextAlignment hAlign vAlign
                          prepAnnotation =<< vaxs
       renderClass crd (AxisClass axes strength _)
          = Draw.tint (let s = realToFrac strength in Draw.Color s s s 1)
              $ foldMap (uncurry Draw.line . crd . axisPosition) axes



-- | When you \"plot\" 'xInterval' / 'yInterval', it is ensured that the (initial) view encompasses 
-- (at least) the specified range.
-- Note there is nothing special about these \"flag\" objects: /any/ 'Plottable' can request a 
-- certain view, e.g. for a discrete point cloud it's obvious and a function defines at least
-- a @y@-range for a given @x@-range. Only use explicit range when necessary.
xInterval, yInterval :: (R, R) -> DynamicPlottable
xInterval (l,r) = DynamicPlottable { 
               relevantRange_x = const . return $ Interval l r
             , relevantRange_y = const mempty
             -- , usesNormalisedCanvas = False
             , isTintableMonochromic = False
             , axesNecessity = 0
             , dynamicPlot = plot }
 where plot _ = Plot mempty mempty
yInterval (b,t) = DynamicPlottable { 
               relevantRange_x = const mempty
             , relevantRange_y = const . return $ Interval b t
             -- , usesNormalisedCanvas = False
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
data TextTK = TextTK { defaultFont :: Draw.Font
                     , txtSize, xAspect, padding, extraTopPad :: R }

prerenderAnnotation :: DiagramTK -> Annotation -> Draw.Image Any
prerenderAnnotation (DiagramTK{ textTools = TextTK{..}, viewScope = GraphWindowSpec{..} }) 
                    (Annotation{..})
       | TextAnnotation (PlainText str) (TextAlignment{..}) <- getAnnotation
       , ExactPlace p₀ <- placement
            = let (rnTextLines, lineWidths) 
                       = unzip . map (Draw.text defaultFont &&& Draw.textWidth defaultFont) 
                            $ lines str
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
                                 in (Draw.translate (case hAlign of 
                                      AlignBottom -> (padding       , y₀-y)
                                      AlignMid    -> (- w/2         , y₀-y)
                                      AlignTop    -> (-(w + padding), y₀-y)
                                     ) %% ) ) [0..] lineWidths rnTextLines
                  p = (px, py)
                   where px = max l' . min r' $ fst p₀
                         py = max b' . min t' $ snd p₀
                         (l', r') = case hAlign of
                           AlignBottom -> (lBound      , rBound - w  )
                           AlignMid    -> (lBound + w/2, rBound - w/2)
                           AlignTop    -> (lBound + w  , rBound      )
                         (b', t') = case vAlign of
                           AlignBottom -> (bBound      , tBound - h  )
                           AlignMid    -> (bBound + h/2, tBound - h/2)
                           AlignTop    -> (bBound + h  , tBound      )
                         w = ζx * width; h = ζy * height
              in Draw.translate p <> Draw.scale ζx ζy 
                     %% Draw.tint (Draw.Color 0.5 0.5 0.5 1) fullText
        


loadFont :: IO Draw.Font
loadFont = do
   let rdTTFfname = takeWhile(/='"') . tail . dropWhile(/='"')
   fontsList <- fmap (map rdTTFfname . lines)
        $ readProcess "bash" ["-c", "fc-list -v :lang=en | grep ttf"] ""
   let (fontChoice:_) = [ font
                        | preference <- [ "mplus-2c-medium.ttf"
                                        , "LiberationSans-Regular.ttf"
                                        , "FreeSans.ttf"
                                        , "DejaVuSans.ttf"
                                        , "DroidSans.ttf" 
                                        , "elvetica"
                                        , "rial"
                                        , "" ]
                        , font <- fontsList
                        , preference`isInfixOf`font            ]
   Draw.openFont fontChoice



infixl 7 `provided`
provided :: Monoid m => m -> Bool -> m
provided m True = m
provided m False = mempty


lg :: Floating a => a -> a
lg x = log x / log 10


instance (Monoid v) => Semigroup (Draw.Image v) where
  (<>) = mappend
instance Semigroup (Draw.Affine) where
  (<>) = mappend




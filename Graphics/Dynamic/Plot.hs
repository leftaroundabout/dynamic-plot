{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE TupleSections           #-}

module Graphics.Dynamic.Plot where


import Graphics.DrawingCombinators ((%%), R, R2)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as OpenGL
import Graphics.Rendering.OpenGL (($=))

import Control.Monad
import Control.Applicative
import Control.Category
import Control.Arrow

import Prelude hiding((.), id)

import Data.List (intercalate, isPrefixOf, isInfixOf, find)
import Data.Maybe
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Function (on)
import qualified Data.Map.Lazy as Map

import Data.IORef

import System.IO
import System.Exit
import Data.Time




data GraphWindowSpec = GraphWindowSpec {
      lBound, rBound, bBound, tBound :: R
    , xResolution, yResolution :: Int
  } deriving (Show)

moveStepRel :: (R, R)  -- ^ Relative translation @(Δx/w, Δy/h)@.
            -> (R, R)  -- ^ Relative zoom.
            -> GraphWindowSpec -> GraphWindowSpec
moveStepRel (δx,δy) (ζx,ζy) (GraphWindowSpec l r b t xRes yRes)
  = GraphWindowSpec l' r' b' t' xRes yRes
 where qx = (r-l)/2      ; qy = (t-b)/2
       mx'= l + qx*(1+δx); my'= b + qy*(1+δy) 
       qx'= qx / ζx      ; qy'= qy / ζy
       l' = mx' - qx'    ; b' = my' - qy'
       r' = mx' + qx'    ; t' = my' + qy'

type Interval = (R, R)

unionClosure :: Interval -> Interval -> Interval
unionClosure (l₁, u₁) (l₂, u₂) = (min l₁ l₂, max u₁ u₂)

type Plot = Draw.Image Any

data DynamicPlottable = DynamicPlottable { 
        relevantRange_x :: Maybe Interval
      , relevantRange_y :: Interval -> Maybe Interval
      , usesNormalisedCanvas :: Bool
      , isTintableMonochromic :: Bool
      , axesNecessity :: Double
      , dynamicPlot :: GraphWindowSpec -> Plot
  }


initScreen :: IO ()
initScreen = do
    True <- GLFW.initialize
    True <- GLFW.openWindow (OpenGL.Size defResX defResY) [] GLFW.Window
    GLFW.windowTitle $= "Plot"
    return ()
                

plotWindow :: [DynamicPlottable] -> IO GraphWindowSpec
plotWindow graphs = do
   initScreen
   
   viewTgt   <- newIORef $ autoDefaultView graphs
   viewState <- newIORef =<< readIORef viewTgt
   
   t₀ <- getCurrentTime
   lastFrameTime <- newIORef t₀
   
   let minKeyImpact = 0.05
   
   keyImpactState <- newIORef $ Map.fromList [ (ka, (t₀, minKeyImpact)) | ka<-[MoveLeft .. ZoomOut_y] ]
   
   done      <- newIORef False
   
   let grey = Draw.Color 0.5 0.5 0.5 0.5
   let mainLoop = do
           t <- getCurrentTime
           δt <- fmap (diffUTCTime t) $ readIORef lastFrameTime
           writeIORef lastFrameTime t
   
           currentView@(GraphWindowSpec{..}) <- do
                   vt <- readIORef viewTgt
                   modifyIORef viewState $ \vo 
                        -> let a%b = let η = min 1 $ 2 * realToFrac δt in η*a + (1-η)*b
                           in GraphWindowSpec (lBound vt % lBound vo) (rBound vt % rBound vo)
                                              (bBound vt % bBound vo) (tBound vt % tBound vo)
                                              (xResolution vt) (yResolution vt)
                   readIORef viewState
           let normaliseView = (Draw.scale xUnZ yUnZ <> Draw.translate (-x₀,-y₀) %%)
                  where xUnZ = 1/w; yUnZ = 1/h
                        w = (rBound - lBound)/2; h = (tBound - bBound)/2
                        x₀ = lBound + w; y₀ = bBound + h
               renderComp (DynamicPlottable{..})
                  = (if usesNormalisedCanvas then id
                      else normaliseView ) . Draw.tint grey $ dynamicPlot currentView
           render . mconcat $ map renderComp graphs
           GLFW.swapBuffers
           GLFW.sleep 0.01
           GLFW.pollEvents
           ($mainLoop) . unless =<< readIORef done
   
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
           when (state==GLFW.Press) $ do
              case defaultKeyMap key of
                Just QuitProgram -> writeIORef done True
                Just movement    -> do
                   impact <- keyImpact movement
                   modifyIORef viewTgt $ case movement of
                    MoveUp    -> moveStepRel (0,  impact) (1, 1)
                    MoveDown  -> moveStepRel (0, -impact) (1, 1)
                    MoveLeft  -> moveStepRel (-impact, 0) (1, 1)
                    MoveRight -> moveStepRel (impact , 0) (1, 1)
                    ZoomIn_x  -> moveStepRel (0, 0)   (1+impact, 1)
                    ZoomOut_x -> moveStepRel (0, 0)   (1-impact/2, 1)
                    ZoomIn_y  -> moveStepRel (0, 0)   (1, 1+impact/2)
                    ZoomOut_y -> moveStepRel (0, 0)   (1, 1-impact/2)
                _ -> return ()
           
   GLFW.windowCloseCallback $= do
           writeIORef done True
           return True
                 
   
   -- putStrLn "Enter Main loop..."
   
   mainLoop
   
   -- putStrLn "Done."
   
   GLFW.terminate
   
   readIORef viewState


autoDefaultView :: [DynamicPlottable] -> GraphWindowSpec
autoDefaultView graphs = finalise . flip (foldr yRanged) graphs . (, Nothing) 
                         . fromMaybe (-1, 1) $ foldr xRanged Nothing graphs
 where xRanged (DynamicPlottable {..}) Nothing = relevantRange_x
       xRanged (DynamicPlottable {..}) (Just oldrng) = fmap (unionClosure oldrng) relevantRange_x
       yRanged (DynamicPlottable {..}) (xrng, Nothing) = (xrng, relevantRange_y xrng)
       yRanged (DynamicPlottable {..}) (xrng, Just oldrng) = (xrng, fmap (unionClosure oldrng) $ relevantRange_y xrng)
       finalise ((l,r), Nothing) = GraphWindowSpec l r (-1) 1 defResX defResY
       finalise ((l,r), Just (b,t)) = GraphWindowSpec l r b t defResX defResY


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




fnPlot :: (R -> R) -> DynamicPlottable
fnPlot f = DynamicPlottable{
               relevantRange_x = Nothing
             , relevantRange_y = yRangef
             , usesNormalisedCanvas = False
             , isTintableMonochromic = True
             , axesNecessity = 1
             , dynamicPlot = plot }
 where yRangef (l, r) = Just . (minimum &&& maximum) $ map f [l, l + (r-l)/8 .. r]
       plot (GraphWindowSpec{..}) = curve
        where δx = (rBound - lBound) * 2 / fromIntegral xResolution
              curve = trace [ (x, f x) | x<-[lBound, lBound+δx .. rBound] ]
              trace (p:q:ps) = Draw.line p q <> trace (q:ps)
              trace _ = mempty




data AxesStyle = DynamicAxesStyle
data DynamicAxes = DynamicAxes { yAxisClasses, xAxisClasses :: [AxisClass] }
data AxisClass = AxisClass { visibleAxes :: [Axis], axisStrength :: Double }
data Axis = Axis { axisPosition :: R } --, decPrecision :: Int }

crtDynamicAxes :: GraphWindowSpec -> DynamicAxes
crtDynamicAxes (GraphWindowSpec {..}) = DynamicAxes yAxCls xAxCls
 where [yAxCls, xAxCls] = zipWith3 directional 
                        [lBound, bBound] [rBound, tBound] [xResolution, yResolution]
       directional l u res = [lvl₀] -- takeWhile ((>0.1) . axisStrength) . scanl purgeDups [] $ aCls
        where -- aCls =  
              span = u - l
              upDecaSpan = 10**(ceil $ lg span)
              pixelScale = span / (fromIntegral res * upDecaSpan)
              baseDecaval = l₀uDSdiv * (ceil $ l * l₀uDSdiv / upDecaSpan)
              lvl₀ = AxisClass [Axis v  | i<-[0 .. l₀uDSdiv], let v=(baseDecaval + i*l₀aSpc), v<u ] 0.3
              l₀aSpc = upDecaSpan / l₀uDSdiv
              l₀uDSdiv = last . takeWhile (\d -> pixelScale * estimAxislabelSize < 1/d )
                                . join $ iterate (map(*10)) [1, 2, 5]
              ceil = fromIntegral . ceiling


dynamicAxes :: DynamicPlottable
dynamicAxes = DynamicPlottable { 
               relevantRange_x = Nothing
             , relevantRange_y = const Nothing
             , usesNormalisedCanvas = False
             , isTintableMonochromic = True
             , axesNecessity = -1
             , dynamicPlot = plot }
 where plot gwSpec@(GraphWindowSpec{..}) 
                = foldMap (renderClass $ \x -> ((x, bBound), ((x, tBound)))) yAxCls
                 <> foldMap (renderClass $ \y -> ((lBound, y), ((rBound, y)))) xAxCls
        where (DynamicAxes yAxCls xAxCls) = crtDynamicAxes gwSpec
       renderClass crd (AxisClass axes strength)
          = foldMap (uncurry Draw.line . crd . axisPosition) axes
 
estimAxislabelSize = 80


lg :: Floating a => a -> a
lg x = log x / log 10



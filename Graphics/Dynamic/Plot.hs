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
import Data.Function (on)

import Data.IORef

import System.IO
import System.Exit




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
   
   viewState <- newIORef $ autoDefaultView graphs
   done      <- newIORef False
   
   let grey = Draw.Color 0.5 0.5 0.5 0.5
   let mainLoop = do
           currentView <- readIORef viewState
           render . mconcat $ map (Draw.tint grey . ($currentView) . dynamicPlot) graphs
           GLFW.swapBuffers
           GLFW.sleep 0.1
           GLFW.pollEvents
           ($mainLoop) . unless =<< readIORef done
   
   GLFW.keyCallback $= \key state -> do
           let keyStepSize = 0.1
           when (state==GLFW.Press) $ do
              case defaultKeyMap key of
                Just QuitProgram -> writeIORef done True
                Just movement    -> modifyIORef viewState $ case movement of
                    MoveUp    -> moveStepRel (0,  keyStepSize) (1, 1)
                    MoveDown  -> moveStepRel (0, -keyStepSize) (1, 1)
                    MoveLeft  -> moveStepRel (keyStepSize,  0) (1, 1)
                    MoveRight -> moveStepRel (-keyStepSize, 0) (1, 1)
                    ZoomIn_x  -> moveStepRel (0, 0)   (1+keyStepSize, 1)
                    ZoomOut_x -> moveStepRel (0, 0)   (1-keyStepSize, 1)
                    ZoomIn_y  -> moveStepRel (0, 0)   (1, 1+keyStepSize)
                    ZoomOut_y -> moveStepRel (0, 0)   (1, 1-keyStepSize)
                _ -> return ()
              view <- readIORef viewState
              putStrLn $ "View: " ++ show view
           
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
render = Draw.render . (Draw.scale (realToFrac defResY / realToFrac defResX) 1 %%)

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

defaultKeyMap :: GLFW.Key -> Maybe KeyAction
defaultKeyMap (GLFW.CharKey 'k') = Just MoveUp
defaultKeyMap (GLFW.CharKey 'j') = Just MoveDown
defaultKeyMap (GLFW.CharKey 'h') = Just MoveLeft
defaultKeyMap (GLFW.CharKey 'l') = Just MoveRight
defaultKeyMap (GLFW.CharKey 'b') = Just ZoomIn_x
defaultKeyMap (GLFW.CharKey 'n') = Just ZoomOut_x
defaultKeyMap (GLFW.CharKey 'i') = Just ZoomIn_y
defaultKeyMap (GLFW.CharKey 'o') = Just ZoomOut_y
defaultKeyMap (GLFW.SpecialKey GLFW.ESC) = Just QuitProgram
defaultKeyMap _ = Nothing




fnPlot :: (R -> R) -> DynamicPlottable
fnPlot f = DynamicPlottable{
               relevantRange_x = Nothing
             , relevantRange_y = yRangef
             , usesNormalisedCanvas = False
             , isTintableMonochromic = True
             , dynamicPlot = plot }
 where yRangef (l, r) = Just . (minimum &&& maximum) $ map f [l, l + (r-l)/8 .. r]
       plot (GraphWindowSpec{..}) = curve
        where δx = (rBound - lBound) * 2 / fromIntegral xResolution
              curve = trace [ (x, f x) | x<-[lBound, lBound+δx .. rBound] ]
              trace (p:q:ps) = Draw.line p q <> trace (q:ps)
              trace _ = mempty

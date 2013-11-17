{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE TupleSections           #-}

module Graphics.Dynamic.Plot where


import Graphics.DrawingCombinators ((%%))
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
      lBound, rBound, bBound, tBound :: Double
    , xResolution, yResolution :: Int
  }

moveStepRel :: (Double, Double)  -- ^ Relative translation @(Δx/w, Δy/h)@.
            -> (Double, Double)  -- ^ Relative zoom.
            -> GraphWindowSpec -> GraphWindowSpec
moveStepRel (δx,δy) (ζx,ζy) (GraphWindowSpec l r b t xRes yRes)
  = GraphWindowSpec l' r' b' t' xRes yRes
 where qx = (r-l)/2      ; qy = (t-b)/2
       mx'= l + qx*(1+δx); my'= b + qy*(1+δy) 
       qx'= qx / ζx      ; qy'= qy / ζy
       l' = mx' - qx'    ; b' = my' - qy'
       r' = mx' + qx'    ; t' = my' + qy'

type Interval = (Double, Double)

unionClosure :: Interval -> Interval -> Interval
unionClosure (l₁, u₁) (l₂, u₂) = (min l₁ l₂, max u₁ u₂)

type Plot = Draw.Image Any

data DynamicPlottable = DynamicPlottable { 
        relevantRange_x :: Maybe Interval
      , relevantRange_y :: Interval -> Maybe Interval
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
   
   let mainLoop = do
           currentView <- readIORef viewState
           render . mconcat $ map (($currentView) . dynamicPlot) graphs
           GLFW.sleep 0.1
           GLFW.pollEvents
           ($mainLoop) . unless =<< readIORef done
   
   GLFW.keyCallback $= \key state -> do
           let keyStepSize = 0.1
           when (state==GLFW.Press) $
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
           
   
   mainLoop
   
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
render = Draw.clearRender . (Draw.scale (realToFrac defResY / realToFrac defResX) 1 %%)

defResX, defResY :: Integral i => i
defResX = 640
defResY = 320


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
defaultKeyMap _ = Nothing


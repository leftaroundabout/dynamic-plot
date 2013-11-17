{-# LANGUAGE ScopedTypeVariables     #-}
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

type Interval = (Double, Double)

unionClosure :: Interval -> Interval -> Interval
unionClosure (l₁, u₁) (l₂, u₂) = (min l₁ l₂, max u₁ u₂)

type Plot = Draw.Image Any

data DynamicPlottable = DynamicPlottable { 
        relevantRange_x :: Maybe Interval
      , relevantRange_y :: Interval -> Maybe Interval
      , dynamicPlot :: GraphWindowSpec -> Plot
  }

defResX, defResY :: Integral i => i
defResX = 640
defResY = 320


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
   return undefined
 


autoDefaultView :: [DynamicPlottable] -> GraphWindowSpec
autoDefaultView graphs = finalise . flip (foldr yRanged) graphs . (, Nothing) 
                         . fromMaybe (-1, 1) $ foldr xRanged Nothing graphs
 where xRanged (DynamicPlottable newrng _ _) Nothing = newrng
       xRanged (DynamicPlottable newrng _ _) (Just oldrng) = fmap (unionClosure oldrng) newrng
       yRanged (DynamicPlottable _ newrng _) (xrng, Nothing) = (xrng, newrng xrng)
       yRanged (DynamicPlottable _ newrng _) (xrng, Just oldrng) = (xrng, fmap (unionClosure oldrng) $ newrng xrng)
       finalise ((l,r), Nothing) = GraphWindowSpec l r (-1) 1 defResX defResY
       finalise ((l,r), Just (b,t)) = GraphWindowSpec l r b t defResX defResY



-- |
-- Module      : Graphics.Dynamic.Plot.R2.Gtk
-- Copyright   : (c) Justus Sagemüller 2022-2023
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

module Graphics.Dynamic.Plot.R2.Gtk (
          module Graphics.Dynamic.Plot.R2
        -- * Interactive display
        , plotWindow, plotWindow'
        ) where

import Graphics.Dynamic.Plot.R2
import Graphics.Dynamic.Plot.Internals

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

import qualified Diagrams.Backend.Gtk as BGTK
import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk ( AttrOp((:=)) )
import qualified Graphics.UI.Gtk.Gdk.EventM as Event
import qualified System.Glib.Signals (on)

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

import Data.IORef

import System.IO
import System.Exit
import System.Process
import Data.Time


-- | Plot some plot objects to a new interactive GTK window. Useful for a quick
--   preview of some unknown data or real-valued functions; things like selection
--   of reasonable view range and colourisation are automatically chosen.
--   
--   Example:
-- 
--   <<images/examples/HelloWorld.gif>>
--  
--   The individual objects you want to plot can be evaluated in multiple threads, so
--   a single hard calculatation won't freeze the responsitivity of the whole window.
--   Invoke e.g. from @ghci +RTS -N4@ to benefit from this.
--   
--   ATTENTION: the window may sometimes freeze, especially when displaying 
--   complicated functions with 'fnPlot` from ghci. This is apparently
--   a kind of deadlock problem with one of the C libraries that are invoked,
--   At the moment, we can recommend no better solution than to abort and restart ghci
--   (or what else you use – iHaskell kernel, process, ...) if this occurs.
plotWindow :: [DynamicPlottable] -> IO GraphWindowSpec
plotWindow = plotWindow' def

-- | Like 'plotWindow', but with explicit specification how the window is supposed
--   to show up. ('plotWindow' uses the default configuration, i.e. 'def'.)
plotWindow' :: ViewportConfig -> [DynamicPlottable] -> IO GraphWindowSpec
plotWindow' viewportConfig [] = plotWindow' viewportConfig [dynamicAxes]
plotWindow' viewportConfig givenPlotObjs = runInBoundThread $ do
   
   let defColourScheme = defaultColourScheme
       tintedPlotObjs = chooseAutoTints givenPlotObjs
   
   viewState <- newIORef $ autoDefaultView viewportConfig tintedPlotObjs
   viewTgt <- newIORef =<< readIORef viewState
   let objAxisLabels = concat $ _axisLabelRequests<$>givenPlotObjs
   viewTgtGlobal <- newMVar . (,objAxisLabels) =<< readIORef viewState
   screenResolution <- newIORef (viewportConfig^.xResV, viewportConfig^.yResV)
   let viewConstraint = flip (foldr _viewportConstraint) givenPlotObjs

   let screenCoordsToData (sx,sy) = do
         GraphWindowSpecR2{..} <- readIORef viewState
         let snx = sx / fromIntegral xResolution
             sny = sy / fromIntegral yResolution
         return (lBound + snx*(rBound-lBound), tBound - sny*(tBound-bBound))
   
   dgStore <- newIORef mempty
   
   (plotObjs, cancelWorkers) :: ([ObjInPlot], IO ()) <- do
       let assignPlObjPropties :: [DynamicPlottable] -> Necessity
                                    -> IO [(ObjInPlot, ThreadId)]
           assignPlObjPropties [] axesNeed
              | axesNeed > 0  = assignPlObjPropties [tint Dia.grey dynamicAxes] (-1)
              | otherwise     = return []
           assignPlObjPropties (o:os) axn = do
              newDia <- newEmptyMVar
              newMouseEvs <- newEmptyMVar
              workerId <- forkIO $ objectPlotterThread o viewTgtGlobal newMouseEvs newDia
              stableView <- newIORef Nothing
              ((ObjInPlot stableView newDia newMouseEvs cl o, workerId) :)
                     <$> assignPlObjPropties os (axn + o^.axesNecessity)
            where cl | TrueColour c₀:_ <- o^.inherentColours
                                  = Just $ Dia.opaque c₀
                     | SymbolicColour c₀:_ <- o^.inherentColours 
                                  = Just $ defColourScheme c₀
                     | otherwise  = Nothing
       (pObs, workerId) <- unzip <$> assignPlObjPropties tintedPlotObjs 0
       return ( sortBy (comparing $ _occlusiveness . _originalPlotObject) pObs
              , forM_ workerId killThread )
   
   
   GTK.initGUI
   window <- GTK.windowNew
   
   mouseAnchor <- newIORef Nothing
   mousePressedAt <- newIORef Nothing
                 
   refreshDraw <- do
       drawA <- GTK.drawingAreaNew
       GTK.onExpose drawA $ \_ -> do
                (canvasX,canvasY) <- GTK.widgetGetSize drawA
                modifyIORef viewTgt
                   $ \view -> viewConstraint $ view{ xResolution = fromIntegral canvasX
                                                   , yResolution = fromIntegral canvasY }

                dia <- readIORef dgStore
                    
                let scaledDia = Dia.bg (case viewportConfig^.plotBackground of
                                         Just bgc -> bgc
                                         Nothing -> Dia.black)
                                . Dia.scaleX (fromInt canvasX / 2)
                                . Dia.scaleY (-fromInt canvasY / 2)
                                . Dia.translate (1 ^& (-1))
                                . Dia.withEnvelope (Dia.rect 2 2 :: PlainGraphicsR2)
                                . (viewportConfig^.graphicsPostprocessing)
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
       
       GTK.on drawA GTK.buttonPressEvent . Event.tryEvent $ do
            Event.eventButton >>= guard.(==defaultEditButton)
            (pressX,pressY) <- liftIO . screenCoordsToData =<< Event.eventCoordinates
            liftIO . writeIORef mousePressedAt $ Just (pressX,pressY)
            let event = MouseEvent (pressX^&pressY) (pressX^&pressY)
            liftIO . forM_ plotObjs $ _mouseEventsForObj >>> \mevs -> do
                   tryTakeMVar mevs >>= \case
                      Nothing -> putMVar mevs $ Interactions [] (Just event)
                      Just (Interactions qe _)
                              -> putMVar mevs $ Interactions qe (Just event)
       GTK.on drawA GTK.buttonReleaseEvent . Event.tryEvent $ do
            Event.eventButton >>= guard.(==defaultEditButton)
            (relX,relY) <- liftIO . screenCoordsToData =<< Event.eventCoordinates
            liftIO (readIORef mousePressedAt) >>= \case
             Just (pressX,pressY) -> liftIO $ do
                let event = MouseEvent (pressX^&pressY) (relX^&relY)
                forM_ plotObjs $ _mouseEventsForObj >>> \mevs -> do
                   tryTakeMVar mevs >>= \case
                      Nothing -> putMVar mevs $ Interactions [event] Nothing
                      Just (Interactions qe _)
                              -> putMVar mevs $ Interactions (event:qe) Nothing
             Nothing -> mzero
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
             Nothing -> liftIO (readIORef mousePressedAt) >>= \case
                Just (pressX,pressY) -> do
                  (curX,curY) <- liftIO . screenCoordsToData =<< Event.eventCoordinates
                  let event = MouseEvent (pressX^&pressY) (curX^&curY)
                  liftIO . forM_ plotObjs $ _mouseEventsForObj >>> \mevs -> do
                    tryTakeMVar mevs >>= \case
                      Nothing -> putMVar mevs $ Interactions [] (Just event)
                      Just (Interactions qe _)
                              -> putMVar mevs $ Interactions qe (Just event)
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
                      , GTK.windowDefaultWidth := viewportConfig^.xResV
                      , GTK.windowDefaultHeight := viewportConfig^.yResV
                      , GTK.containerChild := drawA
                      ]
       
       GTK.widgetShowAll window
       
       return $ GTK.widgetQueueDraw drawA
       
   
   t₀ <- getCurrentTime
   lastFrameTime <- newIORef t₀
   
   
   let refreshScreen = do
           currentView@(GraphWindowSpecR2{..}) <- readIORef viewState
           let textTK txSiz asp = TextTK defaultTxtStyle txSiz asp 0.2 0.2
               renderComp plotObj = do
                   plt <- tryTakeMVar (plotObj^.newPlotView) >>= \case
                       Nothing -> fmap snd <$> readIORef (plotObj^.lastStableView)
                       newDia -> do
                           writeIORef (plotObj^.lastStableView) newDia
                           return $ snd <$> newDia
                   case plt of
                    Nothing -> return mempty
                    Just (Plot{..}, objLegend) -> do
                       renderedAnnot
                           <- renderAnnotationsForView currentView _plotAnnotations
                       return (normaliseView currentView
                                     $ renderedAnnot <> _getPlot, objLegend)

           (thisPlots, thisLegends)
                 <- unzip . reverse <$> mapM renderComp (reverse plotObjs)
           let thePlot = mconcat thisPlots
           theLegend <- prerenderLegend (textTK 10 1) colourScheme
                           (LegendDisplayConfig Dia.absolute)
                                   $ concat (fst<$>thisLegends)
                   
           writeIORef dgStore $ maybe mempty
                            (\l -> l & Dia.scaleX (0.1 / sqrt (fromIntegral xResolution))
                                     & Dia.scaleY (0.1 / sqrt (fromIntegral yResolution)) 
                                     & (`Dia.place`(0.75^&0.75)) ) theLegend
                                <> thePlot
                                                    
           refreshDraw
           
   let mainLoop = do
           t <- getCurrentTime
           δt <- fmap (diffUTCTime t) $ readIORef lastFrameTime
           writeIORef lastFrameTime t
   
           do vt <- readIORef viewTgt
              modifyMVar_ viewTgtGlobal $ return . first (const vt)
              modifyIORef viewState $ \vo -> 
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
        cancelWorkers
        GTK.mainQuit
                 
   
   GTK.timeoutAdd mainLoop 50
   

   GTK.mainGUI
   
   readIORef viewState




data ScrollAction = ScrollZoomIn | ScrollZoomOut

defaultScrollBehaviour :: Event.ScrollDirection -> ScrollAction
defaultScrollBehaviour Event.ScrollUp = ScrollZoomIn
defaultScrollBehaviour Event.ScrollDown = ScrollZoomOut

defaultDragButton :: Event.MouseButton
defaultDragButton = Event.MiddleButton

defaultEditButton :: Event.MouseButton
defaultEditButton = Event.LeftButton

scrollZoomStrength :: Double
scrollZoomStrength = 1/20




-- |
-- Module      : Graphics.Dynamic.Plot.R2
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

module Graphics.Dynamic.Plot.R2 (
        -- * Display
        -- ** Static
          plotPrerender
        -- ** Interactive
        --
        -- $gtkPlotting
        --
        -- * Plottable objects
        -- ** Class  
        , Plottable(..)
        -- ** Simple function plots 
        , fnPlot, paramPlot
        , continFnPlot
        , tracePlot
        , lineSegPlot
        , linregressionPlot
        , colourPaintPlot
        , PlainGraphicsR2
        , shapePlot
        , diagramPlot
        -- ** Multiple objects in one plot
        , plotMultiple
        -- ** Computation in progress
        , plotLatest
        -- * Plot-object attributes
        -- ** Colour
        , tint, autoTint
        -- ** Legend captions
        , legendName
        , plotLegendPrerender
        -- ** Animation
        , plotDelay, freezeAnim, startFrozen
        -- * Viewport
        -- ** View selection
        , xInterval, yInterval, forceXRange, forceYRange
        , unitAspect
        -- ** Interactive content
        -- $interactiveExplanation
        -- *** Mouse
        , MousePressed (..), MousePress(..), MouseClicks(..)
        , clickThrough, withDraggablePoints, mouseInteractive
        , MouseEvent, clickLocation, releaseLocation
        -- *** Displayed range
        , ViewXCenter(..), ViewYCenter(..), ViewWidth(..), ViewHeight(..)
        -- *** Resolution
        , ViewXResolution(..), ViewYResolution(..)
        -- * Auxiliary plot objects
        , dynamicAxes, noDynamicAxes, xAxisLabel, yAxisLabel
        -- * Types
        -- ** The plot type
        , DynamicPlottable
        , tweakPrerendered
        -- ** Viewport choice
        , ViewportConfig
        -- *** Resolution
        , xResV, yResV
        -- *** Background
        , setSolidBackground
        -- *** Output scaling
        , prerenderScaling
        , PrerenderScaling(..)
        , LegendDisplayConfig
        , legendPrerenderSize
        -- *** General
        , graphicsPostprocessing
        ) where

import Graphics.Dynamic.Plot.R2.Internal

import Graphics.Dynamic.Plot.Internal.Types


-- $gtkPlotting
--
--   The "Graphics.Dynamic.Plot.R2.Gtk" module contains
--   'Graphics.Dynamic.Plot.R2.Gtk.plotWindow', which can be used for displaying data in an
--   interactive window.
--   
--   Example:
-- 
--   <<images/examples/HelloWorld.gif>>

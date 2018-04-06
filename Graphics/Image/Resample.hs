-- |
-- Module      : Graphics.Image.Resample
-- Copyright   : (c) Justus Sagemüller 2018
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions

{-# LANGUAGE FlexibleInstances #-}

module Graphics.Image.Resample where

import qualified Codec.Picture as JPix
import qualified Codec.Picture.Types as JPix

import qualified Data.Vector.Storable as SArr

import Control.Monad
import Control.Monad.ST


scaleX2Bilinear :: JPix.Image JPix.PixelRGBA8 -> JPix.Image JPix.PixelRGBA8
scaleX2Bilinear img = runST $ do
   buf <- JPix.newMutableImage wScaled hScaled
   
   forM_ [0 .. hOrig-2] $ \j -> do
      forM_ [0 .. wOrig-2] $ \k -> do
         let orig₀₀ = JPix.pixelAt img j k
             orig₀₁ = JPix.pixelAt img j (k+1)
             orig₁₀ = JPix.pixelAt img (j+1) k
             orig₁₁ = JPix.pixelAt img (j+1) (k+1)
         JPix.writePixel buf (j*2)   (k*2)   $ orig₀₀
         JPix.writePixel buf (j*2)   (k*2+1) $ between orig₀₀ orig₀₁
         JPix.writePixel buf (j*2+1) (k*2)   $ between orig₀₀ orig₁₀
         JPix.writePixel buf (j*2+1) (k*2+1) $ between orig₀₀ orig₁₁
   
   forM_ [0 .. hOrig-2] $ \j -> do
      forM_ [wOrig-1] $ \k -> do
         let orig₀₀ = JPix.pixelAt img j k
             orig₁₀ = JPix.pixelAt img (j+1) k
         JPix.writePixel buf (j*2)   (k*2)   $ orig₀₀
         JPix.writePixel buf (j*2+1) (k*2)   $ between orig₀₀ orig₁₀
   
   forM_ [hOrig-1] $ \j -> do
      forM_ [0 .. wOrig-2] $ \k -> do
         let orig₀₀ = JPix.pixelAt img j k
             orig₀₁ = JPix.pixelAt img j (k+1)
         JPix.writePixel buf (j*2)   (k*2)   $ orig₀₀
         JPix.writePixel buf (j*2)   (k*2+1) $ between orig₀₀ orig₀₁
         
   forM_ [hOrig-1] $ \j -> do
      forM_ [wOrig-1] $ \k -> do
         let orig₀₀ = JPix.pixelAt img j k
         JPix.writePixel buf (j*2) (k*2) $ orig₀₀

   JPix.freezeImage buf
         
 where wOrig = JPix.imageWidth img
       wScaled = 2 * wOrig - 1
       hOrig = JPix.imageHeight img
       hScaled = 2 * hOrig - 1

between :: JPix.PixelRGBA8 -> JPix.PixelRGBA8 -> JPix.PixelRGBA8
between (JPix.PixelRGBA8 r₀ g₀ b₀ a₀) (JPix.PixelRGBA8 r₁ g₁ b₁ a₁)
       = JPix.PixelRGBA8 (r₀`quot`2 + r₁`quot`2)
                         (g₀`quot`2 + g₁`quot`2)
                         (b₀`quot`2 + b₁`quot`2)
                         (a₀`quot`2 + a₁`quot`2)

-- |
-- Module      : Graphics.Image.Resample
-- Copyright   : (c) Justus Sagemüller 2018
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions

{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
         let orig₀₀ = JPix.pixelAt img k     j
             orig₀₁ = JPix.pixelAt img (k+1) j
             orig₁₀ = JPix.pixelAt img k     (j+1)
             orig₁₁ = JPix.pixelAt img (k+1) (j+1)
         JPix.writePixel buf (k*2)   (j*2)   $ orig₀₀
         JPix.writePixel buf (k*2+1) (j*2)   $ between orig₀₀ orig₀₁
         JPix.writePixel buf (k*2)   (j*2+1) $ between orig₀₀ orig₁₀
         JPix.writePixel buf (k*2+1) (j*2+1) $ between (between orig₀₀ orig₁₁)
                                                       (between orig₀₁ orig₁₀)
   
   forM_ [0 .. hOrig-2] $ \j -> do
      forM_ [wOrig-1] $ \k -> do
         let orig₀₀ = JPix.pixelAt img k j
             orig₁₀ = JPix.pixelAt img k (j+1)
         JPix.writePixel buf (k*2)   (j*2)   $ orig₀₀
         JPix.writePixel buf (k*2)   (j*2+1) $ between orig₀₀ orig₁₀
   
   forM_ [hOrig-1] $ \j -> do
      forM_ [0 .. wOrig-2] $ \k -> do
         let orig₀₀ = JPix.pixelAt img k     j
             orig₀₁ = JPix.pixelAt img (k+1) j
         JPix.writePixel buf (k*2)   (j*2)   $ orig₀₀
         JPix.writePixel buf (k*2+1) (j*2)   $ between orig₀₀ orig₀₁
         
   forM_ [hOrig-1] $ \j -> do
      forM_ [wOrig-1] $ \k -> do
         let orig₀₀ = JPix.pixelAt img k j
         JPix.writePixel buf (k*2) (j*2) $ orig₀₀

   JPix.unsafeFreezeImage buf
         
 where wOrig = JPix.imageWidth img
       wScaled = 2 * wOrig - 1
       hOrig = JPix.imageHeight img
       hScaled = 2 * hOrig - 1

refiningScaleX2Bilinear ::
     [(Int,Int)] -> ((Int,Int) -> JPix.PixelRGBA8)
    -> JPix.Image JPix.PixelRGBA8 -> JPix.Image JPix.PixelRGBA8
refiningScaleX2Bilinear hotSpots refineFn loRes = runST (do
    intermediate <- JPix.unsafeThawImage $ scaleX2Bilinear loRes

    alreadyDone <- JPix.unsafeThawImage
           $ JPix.generateImage (\_ _ -> 0::JPix.Pixel8)
                        renderWidth renderHeight
    
    let refineAt (ix,iy) = do
           JPix.writePixel intermediate ix iy $ refineFn (ix,iy)
           JPix.writePixel alreadyDone ix iy 1
        refineBack (ix,iy) = do
           doneBefore <- JPix.readPixel alreadyDone ix iy
           doneBefore==0 ==> refineAt (ix,iy)
    
    forM_ hotSpots $ \(irx,iry) -> do
       irx > 0 ==> do
          refineBack (2*irx - 1, 2*iry)
          iry > 0 ==> refineBack (2*irx - 1, 2*iry - 1)
          iry < loResHeight-1 ==> refineBack (2*irx - 1, 2*iry + 1)
       irx < loResWidth-1 ==> do
          refineAt (2*irx + 1, 2*iry)
          iry > 0 ==> refineBack (2*irx + 1, 2*iry - 1)
          iry < loResHeight-1 ==> refineAt (2*irx + 1, 2*iry + 1)
       iry > 0 ==> refineBack (2*irx, 2*iry - 1)
       iry < loResHeight-1 ==> refineAt (2*irx, 2*iry + 1)
    
    JPix.unsafeFreezeImage intermediate
   )
 where loResWidth = JPix.imageWidth loRes
       loResHeight = JPix.imageHeight loRes
       renderWidth = loResWidth * 2 - 1
       renderHeight = loResHeight * 2 - 1
       infixr 1 ==>
       (==>) = when

    

between :: JPix.PixelRGBA8 -> JPix.PixelRGBA8 -> JPix.PixelRGBA8
between (JPix.PixelRGBA8 r₀ g₀ b₀ a₀) (JPix.PixelRGBA8 r₁ g₁ b₁ a₁)
       = JPix.PixelRGBA8 (r₀`quot`2 + r₁`quot`2)
                         (g₀`quot`2 + g₁`quot`2)
                         (b₀`quot`2 + b₁`quot`2)
                         (a₀`quot`2 + a₁`quot`2)

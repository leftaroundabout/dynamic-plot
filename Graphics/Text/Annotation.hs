-- |
-- Module      : Graphics.Text.Annotation
-- Copyright   : (c) Justus Sagemüller 2015
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
{-# LANGUAGE FlexibleInstances          #-}
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

module Graphics.Text.Annotation where

import Graphics.Dynamic.Plot.Colour
import Graphics.Dynamic.Plot.Internal.Types


import qualified Prelude

import Diagrams.Prelude ((^&), (&), _x, _y, (|||), (===))
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.TwoD.Size as Dia
import qualified Diagrams.TwoD.Types as DiaTypes
import qualified Diagrams.TwoD.Text as DiaTxt
import Diagrams.BoundingBox (BoundingBox)
import qualified Diagrams.BoundingBox as DiaBB
import qualified Diagrams.Backend.Cairo as Cairo
import qualified Diagrams.Backend.Cairo.Text as CairoTxt
    
import Control.Monad.Trans (liftIO)

import qualified Control.Category.Hask as Hask
import Control.Category.Constrained.Prelude hiding ((^))
import Control.Arrow.Constrained hiding ((|||))
import Control.Monad.Constrained

import Control.Lens hiding ((...), (<.>))
import Control.Lens.TH

  
import Data.List (foldl', sort, intercalate, isPrefixOf, isInfixOf, find, zip4)
import qualified Data.Vector as Arr
import Data.Maybe
import Data.Semigroup
import Data.Foldable (fold, foldMap)
import Data.Function (on)

import Data.VectorSpace
import Data.Basis
import Data.AffineSpace
import Data.Manifold.PseudoAffine
import Data.Manifold.TreeCover
import qualified Data.Map.Lazy as Map

import Data.Tagged

import Text.Printf


 

prettyFloatShow :: Int -> Double -> String
prettyFloatShow _ 0 = "0"
prettyFloatShow preci x
    | preci >= 0, preci < 4  = show $ round x
    | preci < 0, preci > -2  = printf "%.1f" x
    | otherwise   = case ceiling (0.01 + lg (abs x/10^^(preci+1))) + preci of
                      0    | preci < 0  -> printf "%.*f"
                                                     (-preci)
                                                      x
                      expn | expn>preci -> printf "%.*f×₁₀%s"
                                                     (expn-preci)
                                                      (x/10^^expn)
                                                          (showExponentAsSuperscript expn)
                           | otherwise  -> printf "%i×₁₀%s"
                                                   (round $ x/10^^expn :: Int)
                                                        (showExponentAsSuperscript expn)
                                      
showExponentAsSuperscript :: Int -> String
showExponentAsSuperscript = map sup . show
 where sup ch = case lookup ch $ zip "0123456789-"
                                     "⁰¹²³⁴⁵⁶⁷⁸⁹⁻" of
                  Just ch -> ch

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
fromPlaintextObj :: TextObj -> String
fromPlaintextObj (PlainText t) = t

data TextAlignment = TextAlignment { hAlign, vAlign :: Alignment } -- , blockSpread :: Bool }
data Alignment = AlignBottom | AlignMid | AlignTop

type TxtStyle = Dia.Style Dia.V2 R

data DiagramTK = DiagramTK { textTools :: TextTK, viewScope :: GraphWindowSpecR2 }
data TextTK = TextTK { txtCairoStyle :: TxtStyle
                     , txtSize, xAspect, padding, extraTopPad :: R }

defaultTxtStyle :: TxtStyle
defaultTxtStyle = mempty & Dia.fontSizeO 9
                         & Dia.fc Dia.grey
                         & Dia.lc Dia.grey


prerenderAnnotation :: DiagramTK -> Annotation -> IO PlainGraphicsR2
prerenderAnnotation (DiagramTK{ textTools = TextTK{..}, viewScope = GraphWindowSpecR2{..} }) 
                    (Annotation{..})
       | TextAnnotation (PlainText str) (TextAlignment{..}) <- getAnnotation
       , ExactPlace p₀ <- placement = do 
              let dtxAlign = DiaTxt.BoxAlignedText
                     (case hAlign of {AlignBottom -> 0; AlignMid -> 0.5; AlignTop -> 1})
                     (case vAlign of {AlignBottom -> 0; AlignMid -> 0.5; AlignTop -> 1})

              rnTextLines <- mapM (CairoTxt.textVisualBoundedIO txtCairoStyle
                                   . DiaTxt.Text mempty dtxAlign )
                               $ lines str
              let lineWidths = map ((/6 {- Magic number ??? -}) .
                                Dia.width) rnTextLines
                  nLines = length lineWidths
                  lineHeight = 1 + extraTopPad + 2*padding
                  ζx = ζy * xAspect
                  ζy = txtSize -- / lineHeight
                  width  = (maximum $ 0 : lineWidths) + 2*padding
                  height = fromIntegral nLines * lineHeight
                  y₀ = case vAlign of
                              AlignBottom -> padding
                              AlignMid    -> 0
                              AlignTop    -> - padding
                  fullText = mconcat $ zipWith3 ( \n w -> 
                                 let y = n' * lineHeight
                                     n' = n - case vAlign of
                                      AlignTop    -> 0
                                      AlignMid    -> fromIntegral nLines / 2
                                      AlignBottom -> fromIntegral nLines
                                 in (Dia.translate $ Dia.r2 (case hAlign of 
                                      AlignBottom -> ( padding, y₀-y )
                                      AlignMid    -> ( 0      , y₀-y )
                                      AlignTop    -> (-padding, y₀-y )
                                     ) ) ) [0..] lineWidths rnTextLines
                  p = px ^& py
                   where px = max l' . min r' $ p₀^._x
                         py = max b' . min t' $ p₀^._y
                         (l', r') = case hAlign of
                           AlignBottom -> (lBound      , rBound - w  )
                           AlignMid    -> (lBound + w/2, rBound - w/2)
                           AlignTop    -> (lBound + w  , rBound      )
                         (b', t') = case vAlign of
                           AlignBottom -> (bBound'      , tBound - 2*h  )
                           AlignMid    -> (bBound' + h/2, tBound - 3*h/2)
                           AlignTop    -> (bBound' + h  , tBound - h    )
                         w = ζx * width; h = 1.5 * ζy * height
                         bBound' = bBound + lineHeight*ζy
              return . Dia.translate p . Dia.scaleX ζx . Dia.scaleY ζy 
                     $ Dia.lc Dia.grey fullText
        




lg :: Floating a => a -> a
lg = logBase 10




data LegendEntry = LegendEntry {
        _plotObjectTitle :: TextObj
      , _plotObjRepresentativeColour :: Maybe PColour
      , _customLegendObject :: Option ()
      }
makeLenses ''LegendEntry

instance HasColour LegendEntry where
  asAColourWith sch = asAColourWith sch . _plotObjRepresentativeColour


prerenderLegend :: TextTK -> ColourScheme -> LegendDisplayConfig
                     -> [LegendEntry] -> IO (Maybe PlainGraphicsR2)
prerenderLegend _ _ _ [] = return mempty
prerenderLegend TextTK{..} cscm layoutSpec l = do
   let bgColour = cscm neutral
   lRends <- forM l `id`\legEntry -> do
          txtR <- CairoTxt.textVisualBoundedIO txtCairoStyle
                       $ DiaTxt.Text mempty (DiaTxt.BoxAlignedText 0 0.5)
                                            (fromPlaintextObj $ legEntry^.plotObjectTitle)
          let h = Dia.height txtR
          return $ Dia.hsep 5 [ Dia.rect h h & Dia.fcA
                                 (asAColourWith cscm legEntry)
                              , txtR
                              ] & Dia.centerXY
                                & Dia.frame 2
                                & Dia.alignL
   let hLine = maximum $ Dia.height <$> lRends
       nLines = case Dia.getSpec (layoutSpec ^. legendPrerenderSize) of
           DiaTypes.V2 _ Nothing -> length l
           DiaTypes.V2 _ (Just hMax) -> floor $ hMax / hLine
       lRends2D = Dia.hcat $ Dia.vcat <$> takes nLines lRends
       w = Dia.width lRends2D
       h = Dia.height lRends2D
   return . pure
          $ ( lRends2D & Dia.centerXY & Dia.translate (3^&3) )
         <> ( Dia.rect (w+1) (h+1) & Dia.fcA (cscm $ paler grey) )
 where takes :: Int -> [a] -> [[a]]
       takes n [] = []
       takes n l = case splitAt n l of
            (h,r) -> h : takes n r


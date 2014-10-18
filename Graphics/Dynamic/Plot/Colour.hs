-- |
-- Module      : Graphics.Dynamic.Plot.Colour 
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions

module Graphics.Dynamic.Plot.Colour where


import qualified Data.Colour as DCol
import Data.Colour (opaque)
import qualified Data.Colour.Names as N
import Data.Colour.CIE hiding (Colour)
import qualified Data.Colour.CIE.Illuminant as Illum


type FColour = DCol.Colour Double
type AColour = DCol.AlphaColour Double

-- | Unlike the typical types such as 'Draw.Color', this one has /semantic/ 
--   more than physical meaning.
data Colour = BaseColour BaseColour
            | Contrast BaseColour
            | Paler Colour
            | CustomColour FColour
            deriving (Eq)
data BaseColour = Neutral -- ^ Either black or white, depending on the context.
                | Red     -- ^ Contrast cyan.
                | Yellow  -- ^ Contrast violet.
                | Green   -- ^ Contrast magenta.
                | Blue    -- ^ Contrast orange.
                deriving (Eq, Show, Enum)


neutral, contrast, grey
 , magenta, red, orange, yellow, green, cyan, blue, violet :: Colour
neutral = BaseColour Neutral
contrast= Contrast Neutral
grey    = paler contrast
magenta = Contrast Green
red     = BaseColour Red
orange  = Contrast Blue
yellow  = BaseColour Yellow
green   = BaseColour Green
cyan    = Contrast Red
blue    = BaseColour Blue
violet  = Contrast Yellow

paler, opposite :: Colour -> Colour
paler = Paler
opposite (BaseColour c) = Contrast c
opposite (Contrast c) = BaseColour c
opposite (Paler c) = Paler $ opposite c
opposite (CustomColour c) = CustomColour $ hueInvert c

type ColourScheme = Colour -> AColour

defaultColourScheme :: ColourScheme
defaultColourScheme (BaseColour Neutral) = opaque N.black
defaultColourScheme (BaseColour Red    ) = opaque N.red
defaultColourScheme (BaseColour Yellow ) = opaque N.yellow
defaultColourScheme (BaseColour Green  ) = opaque N.green
defaultColourScheme (BaseColour Blue   ) = opaque N.blue
defaultColourScheme (Contrast   Neutral) = opaque N.white
defaultColourScheme (Contrast   Red    ) = opaque N.cyan
defaultColourScheme (Contrast   Yellow ) = opaque N.violet
defaultColourScheme (Contrast   Green  ) = opaque N.magenta
defaultColourScheme (Contrast   Blue   ) = opaque N.orange
defaultColourScheme (Paler c) = DCol.dissolve 0.5 $ defaultColourScheme c
defaultColourScheme (CustomColour c) = opaque c


defaultColourSeq :: [Colour] 
defaultColourSeq = cycle [blue, red, green, orange, cyan, magenta, yellow, violet]



hueInvert :: FColour -> FColour
hueInvert c = let (l,a,b) = cieLABView i c
              in cieLAB i l (1-a) (1-b)
 where i = Illum.a

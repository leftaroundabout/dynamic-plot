module Graphics.Dynamic.Plot.Colour where


import qualified Graphics.DrawingCombinators as Draw



-- | Unlike the typical types such as 'Draw.Color', this one has /semantic/ 
--   more than physical meaning.
data Colour = BaseColour BaseColour
            | Contrast BaseColour
            | Paler Colour
            | CustomColour Draw.Color
            deriving (Eq)
data BaseColour = Neutral -- ^ Either black or white, depending on the context.
                | Red     -- ^ Contrast cyan.
                | Yellow  -- ^ Contrast violet.
                | Green   -- ^ Contrast magenta.
                | Blue    -- ^ Contrast orange.
                deriving (Eq, Show, Enum)


magenta, red, orange, yellow, green, cyan, blue, violet :: Colour
magenta = Contrast Green
red     = BaseColour Red
orange  = Contrast Blue
yellow  = BaseColour Yellow
green   = BaseColour Green
cyan    = Contrast Red
blue    = BaseColour Blue
violet  = Contrast Yellow


type ColourScheme = Colour -> Draw.Color

defaultColourScheme :: ColourScheme
defaultColourScheme (BaseColour Neutral) = Draw.Color 0   0   0   1
defaultColourScheme (BaseColour Red    ) = Draw.Color 1   0   0   1
defaultColourScheme (BaseColour Yellow ) = Draw.Color 0.7 0.5 0   1
defaultColourScheme (BaseColour Green  ) = Draw.Color 0   0.8 0   1
defaultColourScheme (BaseColour Blue   ) = Draw.Color 0.2 0.2 1   1
defaultColourScheme (Contrast   Neutral) = Draw.Color 1   1   1   1
defaultColourScheme (Contrast   Red    ) = Draw.Color 0   0.4 0.9 1
defaultColourScheme (Contrast   Yellow ) = Draw.Color 0.4 0   1   1
defaultColourScheme (Contrast   Green  ) = Draw.Color 0.8 0   0.7 1
defaultColourScheme (Contrast   Blue   ) = Draw.Color 0.9 0.2 0   1
defaultColourScheme (Paler c) = Draw.modulate (Draw.Color 1 1 1 0.5) $ defaultColourScheme c
defaultColourScheme (CustomColour c) = c


defaultColourSeq :: [Colour] 
defaultColourSeq = cycle [blue, red, green, magenta, yellow, cyan, violet, orange]

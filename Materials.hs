module Materials where

import qualified Distribution.Simple.Command as Colour

data Colour = RGB
  { colR :: Float,
    colG :: Float,
    colB :: Float
  }

data Material
  = None
  | Mat
      { matDif :: Colour,
        matSpec :: Colour,
        matAmb :: Colour
      }

black :: Colour
black = RGB 0 0 0

white :: Colour
white = RGB 1 1 1

getGreyscale :: Colour -> Float
getGreyscale (RGB r g b) = sqrt (r * r + g * g + b * b)
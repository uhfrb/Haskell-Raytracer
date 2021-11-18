module Materials where

import qualified Distribution.Simple.Command as Colour

data Colour = RGB
  { r :: Float,
    g :: Float,
    b :: Float
  }
  deriving (Show)

data Material
  = None
  | Mat
      { kA :: Float,
        kD :: Float,
        kS :: Float,
        n :: Float
      }
  deriving (Show)

black :: Colour
black = RGB 0 0 0

white :: Colour
white = RGB 1 1 1

getGreyscale :: Colour -> Float
getGreyscale (RGB r g b) = sqrt ((r * r + g * g + b * b) / 3)

scale :: Float -> Colour -> Colour
a `scale` (RGB r g b) = RGB (a * r) (a * g) (a * b)

cAdd :: Colour -> Colour -> Colour
(RGB r1 g1 b1) `cAdd` (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)
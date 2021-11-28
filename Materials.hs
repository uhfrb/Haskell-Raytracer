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
      { kA :: Colour,
        kD :: Colour,
        kS :: Colour,
        nP :: Int
      }
  deriving (Show)

black :: Colour
black = RGB 0 0 0

white :: Colour
white = RGB 1 1 1

clamp :: Colour -> Colour
clamp (RGB r g b) = RGB (min (max 0 r) 1) (min (max 0 g) 1) (min (max 0 b) 1)

getGreyscale :: Colour -> Float
getGreyscale col = sqrt ((r' * r' + g' * g' + b' * b') / 3) where
  (RGB r' g' b') = clamp col

scale :: Float -> Colour -> Colour
a `scale` (RGB r g b) = RGB (a * r) (a * g) (a * b)

cAdd :: Colour -> Colour -> Colour
(RGB r1 g1 b1) `cAdd` (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)

cSub :: Colour -> Colour -> Colour
(RGB r1 g1 b1) `cSub` (RGB r2 g2 b2) = RGB (r1 - r2) (g1 - g2) (b1 - b2)

(RGB r1 g1 b1) `cMul` (RGB r2 g2 b2) = RGB (r1 * r2) (g1 * g2) (b1 * b2)
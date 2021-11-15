module Vectors where

import Control.Arrow (ArrowChoice (left))

data Vec = Vec3 Float Float Float

left3 :: Vec
left3 = Vec3 1 0 0

right3 :: Vec
right3 = inv left3

up3 :: Vec
up3 = Vec3 0 1 0

down3 :: Vec
down3 = inv up3

forward3 :: Vec
forward3 = Vec3 0 0 1

back3 :: Vec
back3 = inv forward3

o3 :: Vec
o3 = Vec3 0 0 0

dot :: Vec -> Vec -> Float
(Vec3 x1 y1 z1) `dot` (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec -> Vec -> Vec
(Vec3 x1 y1 z1) `cross` (Vec3 x2 y2 z2) = Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - x2 * y1)

sqrMagn :: Vec -> Float
sqrMagn v = v `dot` v

magn :: Vec -> Float
magn v = sqrt (sqrMagn v)

scale :: Float -> Vec -> Vec
scale a (Vec3 x y z) = Vec3 (a * x) (a * y) (a * z)

normalize :: Vec -> Vec
normalize v = (1 / magn v) `scale` v

add :: Vec -> Vec -> Vec
add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

inv :: Vec -> Vec
inv v = (-1) `scale` v

sub :: Vec -> Vec -> Vec
sub u v = add u (inv v)

instance Show Vec where
  show (Vec3 x y z) = show (x, y, z)
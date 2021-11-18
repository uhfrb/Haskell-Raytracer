module Lights where
import Materials as Mats
import Vectors

data Light = Point Vec Colour Float

getPos :: Light -> Vec
getPos (Point pos col i) = pos

getInt :: Light -> Vec -> Colour
getInt (Point pos col i) at = (i / sqrMagn (pos `sub` at)) `Mats.scale` col

getCol :: Light -> Colour
getCol (Point _ c _) = c

getI :: Light -> Float
getI (Point _ _ i) = i
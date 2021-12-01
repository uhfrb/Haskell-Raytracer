module Lights where
import Materials as Mats
import Vectors as Vecs

data Light = Point Vec Colour Float | Spotlight Vec Colour Float Vec Int

getPos :: Light -> Vec
getPos (Point pos col i) = pos
getPos (Spotlight pos _ _ _ _) = pos

getInt :: Light -> Vec -> Colour
getInt (Point pos col i) at = (i / sqrMagn (pos `sub` at)) `Mats.scale` col
getInt (Spotlight pos col i dir alpha) at = ((i * fromIntegral (alpha + 2) * max 0 cosTheta ^ alpha) / sqrMagn (pos `sub` at)) `Mats.scale` col
  where
      cosTheta = dir `Vecs.dot` normalize (at `Vecs.sub` pos)

getCol :: Light -> Colour
getCol (Point _ c _) = c
getCol (Spotlight _ c _ _ _) = c

getI :: Light -> Float
getI (Point _ _ i) = i
getI (Spotlight _ _ i _ _) = i
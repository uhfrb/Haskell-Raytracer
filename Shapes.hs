module Shapes where

import Materials
import Rays
import Vectors

data Shape = Sphere {sP :: Vec, sR :: Float, sMat :: Material}

data IntersectionResult = IR
  { iT :: Float,
    iPos :: Vec,
    iN :: Vec,
    iMat :: Material
  }

intersect :: Ray -> Shape -> Maybe IntersectionResult
intersect (Ray e d) (Sphere p r mat)
  | discriminant < 0 = Nothing
  | t_2 < 0 = if t_1 < 0 then Nothing else let i = (e `add` (t_1 `scale` d)) in Just (IR t_1 i (normalize (i `sub` p)) mat)
  | otherwise = let i = (e `add` (t_1 `scale` d)) in Just (IR t_2 i (normalize (i `sub` p)) mat)
  where
    discriminant = b ^ 2 - 4 * a * c
    t_1 = (-b + sqrt discriminant) / (2 * a)
    t_2 = (-b - sqrt discriminant) / (2 * a)
    a = sqrMagn d
    b = 2 `scale` d `dot` (e `sub` p)
    c = sqrMagn (e `sub` p) - r ^ 2

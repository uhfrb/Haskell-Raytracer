module Shapes where

import Materials as Mats
import Rays
import Vectors as Vecs
import qualified Debug.Trace as Debug

zeroMargin :: Float
zeroMargin = 0.0000000000001

data Shape = Sphere {sP :: Vec, sR :: Float, sMat :: Material} | Plane {pD :: Float, pN :: Vec, pMat ::Material}

data IntersectionResult = IR
  { iT :: Float,
    iPos :: Vec,
    iN :: Vec,
    iRV :: Vec,
    iMat :: Material
  }
  deriving (Show)

intersect :: Ray -> Shape -> Maybe IntersectionResult
intersect (Ray e d) (Sphere p r mat)
  | discriminant < 0 = Nothing
  | t_1 < 0 = if t_2 < 0 then Nothing else let i = (e `add` (t_2 `Vecs.scale` d)); n = normalize (i `sub` p) in
    Just (IR t_2 i n (reflect d n) mat)
  | otherwise = let i = (e `add` (t_1 `Vecs.scale` d)); n = normalize (i `sub` p) in
    Just (IR t_1 i n (reflect d n) mat)
  where
    discriminant = b ^ 2 - 4 * a * c
    t_1 = (-b - sqrt discriminant) / (2 * a)
    t_2 = (-b + sqrt discriminant) / (2 * a)
    a = sqrMagn d
    b = (2 `Vecs.scale` d) `dot` (e `sub` p)
    c = sqrMagn (e `sub` p) - r ^ 2
intersect r@(Ray e d) (Plane pd n mat) = if abs denom < zeroMargin || t < 0 then Nothing else let i = e `add` (t `Vecs.scale` d) in
  Just (IR t i n (reflect d n) mat) where
    denom = n `dot` d
    t = (pd - (n `dot` e)) / denom

reflect :: Vec -> Vec -> Vec
reflect d n = normalize (((2 * (d `dot` n)) `Vecs.scale` n) `sub` d)

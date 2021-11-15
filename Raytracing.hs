module Raytracing where

import Data.Maybe
import Materials
import Rays
import Shapes
import Vectors
import View

numRecs :: Int
numRecs = 3

data Camera = Cam
  { cE :: Vec,
    cZ :: Vec,
    cUp :: Vec,
    cW :: Vec,
    cU :: Vec,
    cV :: Vec
  }
  deriving (Show)

calculateCam :: Vec -> Vec -> Vec -> Camera
calculateCam pos up target = Cam pos target up w u (normalize (cross w u))
  where
    w = normalize (sub pos target)
    u = normalize (cross up w)

generateRays :: Float -> Float -> Float -> Float -> Float -> Camera -> [[Ray]]
generateRays l r b t d cam = map rayRow [b .. t]
  where
    rayRow y = map (rayFromPixel y) [l .. r]
    rayFromPixel u v = Ray (cE cam) (normalize (((u + 0.5) `scale` cU cam) `add` ((v + 0.5) `scale` cV cam) `sub` (d `scale` cW cam)))

shootRay :: Ray -> [Shape] -> Maybe IntersectionResult
shootRay r scene = foldl closer Nothing (map (intersect r) scene)
  where
    closer :: Maybe IntersectionResult -> Maybe IntersectionResult -> Maybe IntersectionResult
    closer i1@(Just (IR t1 _ _ _)) i2@(Just (IR t2 _ _ _)) = if t1 < t2 then i1 else i2
    closer Nothing i@(Just (IR {})) = i
    closer ir1 ir2 = ir1

rayTrace :: Int -> [Shape] -> Ray -> Colour
rayTrace recsToGo scene r = if isNothing mir then black else white
  where
    mir = shootRay r scene

render :: [Shape] -> Float -> Float -> Float -> Float -> Float -> Camera -> [[Colour]]
render scene l r b t d cam = map (map (rayTrace numRecs scene)) $ generateRays l r b t d cam

main :: IO ()
main = viewAscii $ render scene (-20) 20 (-15) 15 10 cam
  where
    scene = [Sphere (Vec3 0 0 15) 10 None]
    cam = calculateCam o3 up3 forward3
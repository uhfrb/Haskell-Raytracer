module Raytracing where

import Data.Bits (toIntegralSized)
import Data.Maybe
import qualified Debug.Trace as Debug
import Lights
import Materials as Mats
import Rays
import Shapes
import Vectors as Vecs

numRecs :: Int
numRecs = 5
eps :: Float
eps = 0.001

data Camera = Cam
  { cE :: Vec,
    cZ :: Vec,
    cUp :: Vec,
    cW :: Vec,
    cU :: Vec,
    cV :: Vec
  }
  deriving (Show)

data Scene = Scene
  { obj :: [Shape],
    lights :: [Light]
  }

calculateCam :: Vec -> Vec -> Vec -> Camera
calculateCam pos up target = Cam pos target up w u v
  where
    w = normalize (pos `sub` target)
    u = normalize (up `cross` w)
    v = normalize (w `cross` u)

generateRays :: Float -> Float -> Float -> Float -> Int -> Float -> Float -> Camera -> [[Ray]]
generateRays l r b t h ar d cam = map rayRow [0 .. (h - 1)]
  where
    w = round (fromIntegral h * ar)
    rayRow v = map (rayFromPixel v) [0 .. (w - 1)]
    rayFromPixel :: Int -> Int -> Ray
    rayFromPixel v u = Ray (cE cam) (normalize ((sU `Vecs.scale` cU cam) `add` (sV `Vecs.scale` cV cam) `sub` (d `Vecs.scale` cW cam)))
      where
        sU = l + ((r - l) / fromIntegral w) * (fromIntegral u + 0.5)
        sV = b + ((t - b) / fromIntegral h) * (fromIntegral v + 0.5)

castRay :: Ray -> [Shape] -> Maybe IntersectionResult
castRay r scene = foldl closer Nothing (map (intersect r) scene)
  where
    closer :: Maybe IntersectionResult -> Maybe IntersectionResult -> Maybe IntersectionResult
    closer i1@(Just (IR t1 _ _ _ _)) i2@(Just (IR t2 _ _ _ _)) = if t1 < t2 then i1 else i2
    closer Nothing i@(Just (IR {})) = i
    closer ir1 ir2 = ir1

rayTrace :: Int -> Scene -> Ray -> Colour
rayTrace recsToGo scene r = maybe black (eP scene) mir
  where
    mir = castRay r (obj scene)
    eP sc (IR t p n rv mat) = let pC = evaluatePhong sc mat p (rDir r) n rv; r' = Ray (p `Vecs.add` (eps `Vecs.scale` inv rv)) (inv rv) in
      if cR mat < zeroMargin || recsToGo == 0 then pC else
        --Debug.trace (show (rv, rDir r)) $
        (cR mat `Mats.scale` rayTrace (recsToGo - 1) scene r')
        `cAdd` ((1 - cR mat) `Mats.scale` pC)

evaluatePhong :: Scene -> Material -> Vec -> Vec -> Vec -> Vec -> Colour
evaluatePhong scene None p d n rv = black
evaluatePhong scene (Mat kA kD kS nP _ _) p d n rv = foldl cAdd black (map contr (lights scene))
  where
    contr light = contrDiff `cAdd` contrAmb `cAdd` contrSpec
      where
        pl = getPos light
        l = normalize (pl `sub` p)
        cosTheta = l `dot` n
        cosAlpha = inv l `dot` rv
        contrAmb = (getI light `Mats.scale` kA) `cMul` getCol light
        contrDiff = (isVisible (obj scene) light p * max 0 cosTheta) `Mats.scale` (kD `cMul` getInt light p)
        contrSpec = if cosTheta > 0 then (isVisible (obj scene) light p * (max 0 cosAlpha ^ nP)) `Mats.scale` (kS `cMul` getInt light p) else black

--Debug.trace (show (isVisible scene pl p)) $  $

isVisible :: [Shape] -> Light -> Vec -> Float
isVisible scene l p = if isNothing mir || checkDist mir then 1 else 0
  where
    mir = castRay (Ray (p `add` (eps `Vecs.scale` nd)) nd) scene
    checkDist (Just ir@(IR t _ _ _ _)) = t * t >= sqrMagn d
    d = getPos l `sub` p
    nd = normalize d

render :: Scene -> Float -> Float -> Float -> Float -> Int -> Float -> Float -> Camera -> [[Colour]]
render scene l r b t h ar d cam = map (map (rayTrace numRecs scene)) $ generateRays l r b t h ar d cam
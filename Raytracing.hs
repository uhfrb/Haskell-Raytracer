module Raytracing where

import Data.Bits (toIntegralSized)
import Data.Maybe
import qualified Debug.Trace as Debug
import Lights
import Materials as Mats
import Rays
import Shapes
import Vectors as Vecs
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

data Scene = Scene
  { obj :: [Shape],
    lights :: [Light]
  }

calculateCam :: Vec -> Vec -> Vec -> Camera
calculateCam pos up target = Cam pos target up w u (normalize (w `cross` u))
  where
    w = normalize (pos `sub` target)
    u = normalize (up `cross` w)

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

castRay :: Ray -> Scene -> Maybe IntersectionResult
castRay r scene = foldl closer Nothing (map (intersect r) (obj scene))
  where
    closer :: Maybe IntersectionResult -> Maybe IntersectionResult -> Maybe IntersectionResult
    closer i1@(Just (IR t1 _ _ _)) i2@(Just (IR t2 _ _ _)) = if t1 < t2 then i1 else i2
    closer Nothing i@(Just (IR {})) = i
    closer ir1 ir2 = ir1

rayTrace :: Int -> Scene -> Ray -> Colour
rayTrace recsToGo scene r = maybe black (eP scene) mir
  where
    mir = castRay r scene
    eP sc (IR t p n mat) = evaluatePhong sc mat p (rDir r) n

evaluatePhong :: Scene -> Material -> Vec -> Vec -> Vec -> Colour
evaluatePhong scene None p d n = black
evaluatePhong scene (Mat kA kD kS nP) p d n = foldl cAdd black (map contr (lights scene))
  where
    contr light = contrDiff `cAdd` contrAmb `cAdd` contrSpec
      where
        pl = getPos light
        l = normalize (p `sub` pl)
        cosTheta = n `dot` l
        rv = ((2 * (d `dot` n)) `Vecs.scale` n) `sub` d
        cosAlpha = inv l `dot` rv
        contrAmb = (kA * getI light) `Mats.scale` getCol light
        contrDiff = if isVisible scene pl p then max 0 (kD * cosTheta) `Mats.scale` getInt light p else black
        contrSpec = ((max 0 kS * cosAlpha) ^ nP) `Mats.scale` getInt light p

--Debug.trace (show (isVisible scene pl p)) $ (\x -> Debug.trace (show x) x) $

isVisible :: Scene -> Vec -> Vec -> Bool
isVisible scene p1 p2 = isNothing mir || checkDist mir
  where
    mir = castRay (Ray p1 (normalize d)) scene
    checkDist (Just (IR t _ _ _)) = t * t <= sqrMagn d
    d = p2 `sub` p1

render :: Scene -> Float -> Float -> Float -> Float -> Int -> Float -> Float -> Camera -> [[Colour]]
render scene l r b t h ar d cam = map (map (rayTrace numRecs scene)) $ generateRays l r b t h ar d cam

main :: IO ()
main = viewAscii $ render scene (-1) 1 (-0.5) 0.5 50 (40 / 9) 1 cam
  where
    scene =
      Scene
        [ Sphere (Vec3 1 (-0.25) 3) 1 dW,
          Sphere (Vec3 (-2) 0 5) 2.5 dW
        ]
        [Point (Vec3 3 (-1) 2) white 40, Point (Vec3 (-1) 2 10) white 20]
    cam = calculateCam o3 up3 forward3
    dW = Mat 0.01 1 1 120
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
import Text.XHtml (height)

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

castRay :: Ray -> Scene -> Maybe IntersectionResult
castRay r scene = foldl closer Nothing (map (intersect r) (obj scene))
  where
    closer :: Maybe IntersectionResult -> Maybe IntersectionResult -> Maybe IntersectionResult
    closer i1@(Just (IR t1 _ _ _ _)) i2@(Just (IR t2 _ _ _ _)) = if t1 < t2 then i1 else i2
    closer Nothing i@(Just (IR {})) = i
    closer ir1 ir2 = ir1

rayTrace :: Int -> Scene -> Ray -> Colour
rayTrace recsToGo scene r = maybe black (eP scene) mir
  where
    mir = castRay r scene
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
        contrDiff = if isVisible scene pl p then max 0 cosTheta `Mats.scale` (kD `cMul` getInt light p) else black
        contrSpec = if isVisible scene pl p && cosTheta > 0 then (max 0 cosAlpha ^ nP) `Mats.scale` (kS `cMul` getInt light p) else black

--Debug.trace (show (isVisible scene pl p)) $  $

isVisible :: Scene -> Vec -> Vec -> Bool
isVisible scene p1 p2 = isNothing mir || checkDist mir
  where
    mir = castRay (Ray (p2 `add` (eps `Vecs.scale` nd)) nd) scene
    checkDist (Just ir@(IR t _ _ _ _)) = t * t >= sqrMagn d
    d = p1 `sub` p2
    nd = normalize d

render :: Scene -> Float -> Float -> Float -> Float -> Int -> Float -> Float -> Camera -> [[Colour]]
render scene l r b t h ar d cam = map (map (rayTrace numRecs scene)) $ generateRays l r b t h ar d cam

vpxmin :: Float
vpxmin = -1
vpxmax :: Float
vpxmax = 1
vpymin :: Float
vpymin = -0.5
vpymax :: Float
vpymax = 0.5
vpdist :: Float
vpdist = 1
heightPx :: Int
heightPx = 160
aspectRatio :: Float
aspectRatio = 48/9
cam :: Camera
cam = calculateCam o3 up3 forward3

rtGeneral :: IO ()
rtGeneral = viewAscii $ renderWithStandartVal scene

rtShadowDemo :: IO ()
rtShadowDemo = viewAscii $ renderWithStandartVal shadowScene

rtRefl1 :: IO ()
rtRefl1 = viewAscii $ renderWithStandartVal reflectionScene

rtRefl2 :: IO ()
rtRefl2 = viewAscii $ renderWithStandartVal reflectingSpheres

rtSpotlight :: IO ()
rtSpotlight = viewAscii $ renderWithStandartVal strangeSnowman

renderWithStandartVal :: Scene -> [[Colour]]
renderWithStandartVal scene = render scene vpxmin vpxmax vpymin vpymax heightPx aspectRatio vpdist cam

main :: IO ()
main = viewAscii $ render scene vpxmin vpxmax vpymin vpymax heightPx aspectRatio vpdist cam
  where
    cam = calculateCam o3 up3 forward3


scene :: Scene
scene =
  Scene
    [ Plane (-1) up3 diffWhite,
      Sphere (Vec3 1.5 0 4) 1 diffWhite,
      Sphere (Vec3 (-1.5) 0 4) 1 diffBlue,
      Sphere (Vec3 0.5 0.2 2.5) 0.3 specRed
    ]
    [Point (Vec3 0 0 5) white 20, Point (Vec3 1.5 3.5 2) white 40]-- 

shadowScene :: Scene
shadowScene =
  Scene
    [Plane (-2) back3 diffWhite, Sphere (Vec3 0.4 0 2) 0.5 diffWhite]
    [Point (Vec3 1 0 1.9) white 40]

reflectionScene :: Scene
reflectionScene = Scene
  [ Plane (-1) up3 fullySpec,
    Sphere (Vec3 1.5 0.1 3) 1 diffWhite,
    Sphere (Vec3 (-2.5) 0.1 5) 1 diffWhite,
    Sphere (Vec3 0.5 0.2 2.5) 0.3 diffWhite
    ]
    [Point (Vec3 0 (-0.75) 3) white 40]

reflectingSpheres :: Scene
reflectingSpheres = Scene
  [ Plane (-1) up3 diffWhite,
    Sphere (Vec3 1.5 (-1) 3) 1 fullySpec,
    Sphere (Vec3 (-2.5) 0.1 5) 1 fullySpec,
    Sphere (Vec3 0.5 0.2 2.5) 0.3 fullySpec,
    Sphere (Vec3 (-1.5) 0 7) 2 fullySpec,
    Sphere (Vec3 0 5 2) 3 fullySpec
    ]
    [Point (Vec3 0 (-0.75) 3) white 40]

strangeSnowman :: Scene
strangeSnowman = Scene
  [ Plane (-0.5) up3 diffWhite,
    Sphere (Vec3 (-1) (-0.5) 2) 0.4 diffWhite,
    Sphere (Vec3 (-1) 0.15 2) 0.25 diffWhite,
    Sphere (Vec3 (-1) 0.5 2) 0.1 diffWhite,
    Sphere (Vec3 (-1) 0.15 1.65) 0.1 diffWhite,
    Sphere (Vec3 (-1) 0.15 2.35) 0.1 diffWhite,
    Sphere (Vec3 (-1.35) 0.15 2) 0.1 diffWhite,
    Sphere (Vec3 (-0.65) 0.15 2) 0.1 diffWhite]
  [ Point (Vec3 0.75 0 3) (RGB 0.9 0 0.1) 10, Spotlight (Vec3 (-4) (-0.35) 4) white 40 (normalize (Vec3 1 0 (-0.75))) 200 ]

slPlane :: Scene
slPlane = Scene [Plane (-0.5) up3 diffWhite] [Spotlight (Vec3 (-4) (-0.35) 4) white 40 (normalize (Vec3 1 0 (-0.75))) 200]

diffWhite :: Material
diffWhite = Mat (0.01 `Mats.scale` white) (0.3 `Mats.scale` white) black 0 0  black

diffBlue :: Material
diffBlue = Mat (RGB 0 0 0.02) (RGB 0 0 0.4) (RGB 0.1 0.1 0.5) 10 0  black

specRed :: Material
specRed = Mat (RGB 0.02 0 0.001) (RGB 0.37 0 0.05) (RGB 0.8 0.1 00.1) 100 0.4 (RGB 1 0.1 0.1)

fullySpec :: Material
fullySpec = Mat black black black 0 1 white
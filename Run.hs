import Raytracing
import Materials as Mats
import View
import Vectors
import Shapes
import Lights

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
heightPx = 30
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
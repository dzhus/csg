{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

{-|

Standalone raycaster for CSG objects powered by Gloss.

|-}

import Prelude

import qualified Data.Strict.Maybe as S
import qualified Data.Strict.Tuple as S

import GHC.Float

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import qualified Graphics.Gloss.Data.Point as G
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Graphics.Gloss.Raster.Field hiding (Point)

import System.Console.CmdArgs.Implicit

import Data.CSG
import Data.CSG.Parser
import Data.Vec3 hiding (Vec3)


data InteractionMode = None | Rotate | Pan


-- | World state with observation point parameters and event helpers.
-- Roll is always 0.
data World = World { dist :: Double
                   -- ^ Distance to origin.
                   , pitch :: Double
                   , yaw :: Double
                   -- ^ Yaw of camera as if it was in origin.
                   , target :: Point
                   -- ^ Where camera looks at.
                   , holdPoint :: Maybe (Float, Float)
                   -- ^ Point where mouse button was held down.
                   , mode :: InteractionMode
                   }


-- | Static command line options for caster.
data Options = Options
    { geoFile :: FilePath
    , width :: Int
    , height :: Int
    , pixels :: Int
    , brightRGBA :: (Float, Float, Float, Float)
    -- ^ Color for bright surfaces parallel to view plane (RGBA)
    , darkRGBA :: (Float, Float, Float, Float)
    -- ^ Color for dark surfaces perpendicular to view plane (RGBA)
    }
    deriving (Data, Typeable)


-- | Pixels in meter at unit distance.
scaleFactor :: Double
scaleFactor = 50.0
{-# INLINE scaleFactor #-}


initialDistance :: Double
initialDistance = 5


-- | Initial world.
start :: World
start = World initialDistance 0 0 origin Nothing None


uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d


-- |  Scale deltas between hold and release coordinates by this number.
dragFactor :: Double
dragFactor = pi / 450


-- | Change distance by this amount per one mouse wheel step.
zoomFactor :: Double
zoomFactor = 0.01


-- | Handle mouse drag to change pitch & yaw and mouse wheel to zoom.
handleEvents :: G.Event -> World -> World
handleEvents e w =
    case e of
      G.EventKey (G.MouseButton G.LeftButton) G.Down _ c ->
          w{holdPoint = Just c, mode = Rotate}
      G.EventKey (G.MouseButton G.RightButton) G.Down _ c ->
          w{holdPoint = Just c, mode = Pan}
      G.EventKey (G.MouseButton _) G.Up _ _ ->
          w{holdPoint = Nothing}
      G.EventKey (G.MouseButton G.WheelDown) _ _ _ ->
          w{dist = dist w + zoomFactor}
      G.EventKey (G.MouseButton G.WheelUp) _ _ _ ->
          w{dist = dist w - zoomFactor}
      G.EventKey (G.Char 'r') G.Down _ _ ->
          w{ target = origin
           , yaw = 0
           , pitch = 0
           , dist = initialDistance
           }
      G.EventMotion p@(x, y) ->
          case holdPoint w of
            Nothing -> w
            Just (u, v) ->
                let
                    xdelta = float2Double (x - u) * dragFactor
                    ydelta = float2Double (y - v) * dragFactor
                in
                  case mode w of
                    Rotate -> w{ holdPoint = Just p
                               , yaw = yaw w - xdelta
                               , pitch = pitch w + ydelta
                               }
                    Pan ->
                        let
                            !(_, sX, sY) = buildCartesian (yaw w) (pitch w)
                        in
                          w{ holdPoint = Just p
                           , target = target w <+> (sX .^ xdelta) <-> (sY .^ ydelta)
                           }
                    _ -> w
      _ -> w


-- | Build cartesian axes from yaw and pitch with 0 roll. Angles are
-- in radians.
buildCartesian :: Double -> Double -> (Vec3, Vec3, Vec3)
buildCartesian yaw pitch = (u, v, w)
    where u = fromXYZ (cos yaw * cos pitch, sin yaw * cos pitch, sin pitch)
          v = fromXYZ (- (sin yaw), cos yaw, 0)
          w = u >< v
{-# INLINE buildCartesian #-}


programName :: String
programName = "csg-raycaster"


casterField :: Int
            -- ^ Window width.
            -> Int
            -- ^ Window height.
            -> Int
            -- ^ Pixels per point.
            -> Solid
            -- ^ Solid to show.
            -> Color
            -- ^ Bright color.
            -> Color
            -- ^ Dark color.
            -> IO ()
casterField width height pixels solid bright' dark' =
    let
        display = InWindow programName (width, height) (0, 0)
        makePixel :: World -> G.Point -> Color
        !wS = fromIntegral (width `div` 2)
        !hS = fromIntegral (height `div` 2)
        makePixel !w (x, y) =
            let
                !d = dist w
                !wScale = -(wS * d / scaleFactor)
                !hScale = (hS * d / scaleFactor)
                !(n, sX, sY) = buildCartesian (yaw w) (pitch w)
                !p = n .^ (-d) <+> target w
                ray :: Ray
                !ray = Ray (p
                            <+> (sX .^ (float2Double x * wScale))
                            <+> (sY .^ (float2Double y * hScale)), n)
                !hp = trace solid ray
            in
              case hp of
                (S.:!:) (HitPoint _ (S.Just hn)) _:_ ->
                    mixColors factor (1 - factor) bright' dark'
                    where
                      factor = abs $ double2Float $ invert n .* hn
                _ -> white
        {-# INLINE makePixel #-}
    in
      playField display (pixels, pixels) 5 start makePixel
                    handleEvents
                    (flip const)


-- | Read solid def and program arguments, run the actual caster on
-- success.
main :: IO ()
main =
    let
        sample = Options
                 { geoFile = def &= argPos 0 &= typ "GEO-FILE"
                 , width = 500 &= help "Window width"
                 , height = 500 &= help "Window height"
                 , pixels = 1
                   &= help ("Number of pixels to draw per point," ++
                            "in each dimension")
                 , brightRGBA = (0.9, 0.9, 0.9, 1)
                   &= help ("Color for surface parallel to view plane," ++
                            " with each component within [0..1]")
                   &= typ "R,G,B,A"
                 , darkRGBA = (0, 0, 0, 1)
                   &= help "Color for surface perpendicular to view plane"
                   &= typ "R,G,B,A"
                 }
                 &= program programName
    in do
      Options{..} <- cmdArgs sample
      solid <- parseGeometryFile geoFile
      case solid of
        Right b -> casterField width height pixels b
                   (uncurry4 makeColor brightRGBA)
                   (uncurry4 makeColor darkRGBA)
        Left e -> error $ "Problem when reading solid definition: " ++ e

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

{-|

Standalone raycaster for CSG objects powered by Gloss.

|-}

import Prelude hiding (FilePath)

import Control.Applicative
import qualified Data.Strict.Maybe as S

import GHC.Float
import Data.String
#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid
#endif
import Data.Version

import Data.Word

import Data.Massiv.Array as A hiding ((.^), (.*), generate)
import Graphics.UI.GLUT as G hiding (None)

import Test.QuickCheck hiding ((><))
import Turtle.Options

import Filesystem.Path.CurrentOS

import Data.CSG
import Data.CSG.Parser

import Paths_csg

import UnliftIO.STM

data InteractionMode = None | Rotate | Pan deriving Show

-- | World state with observation point parameters and event helpers.
-- Roll is always 0.
data World = World { dist :: Double
                   -- ^ Distance to origin.
                   , pitch :: Double
                   , yaw :: Double
                   -- ^ Yaw of camera as if it was in origin.
                   , target :: Point
                   -- ^ Where camera looks at.
                   , dragStart :: Maybe Position
                   -- ^ Position where mouse button was held down.
                   , mode :: InteractionMode
                   , solid :: Solid
                   }
             deriving Show


-- | Command line options for caster.
data Options = Options
    { geoFile    :: Maybe FilePath
    , width      :: Int
    , height     :: Int
    , pixels     :: Int
    , brightRGBA :: (Float, Float, Float, Float)
    -- ^ Color for bright surfaces parallel to view plane (RGB)
    , darkRGBA   :: (Float, Float, Float, Float)
    -- ^ Color for dark surfaces perpendicular to view plane (RGB)
    }


optParser :: Parser Options
optParser = Options
  <$> optional (argPath "geo-file" "Geometry definition file")
  <*> (optInt "width" 'w' "Window width" <|> pure 1000)
  <*> (optInt "height" 'h' "Window height" <|> pure 1000)
  <*> (optInt "pixels" 'p' "Pixels per ray (set to 1 for top quality)"
       <|> pure 1)
  <*> (optRead "bright-color" 'b'
       (fromString $
        "Color for bright surfaces (parallel to the view plane) " <>
        "as (R, G, B, A) with each component between 0.0 and 1.0")
       <|> pure (0.9, 0.9, 0.9, 1))
  <*> (optRead "dark-color" 'd' "Color for dark surfaces"
       <|> pure (0, 0, 0, 1))


-- | The factor between distance and view port width/height.
scaleFactor :: Double
scaleFactor = 225.0
{-# INLINE scaleFactor #-}


initialDistance :: Double
initialDistance = 225


-- | Initial world.
start :: Solid -> World
start s = World initialDistance (- pi / 6) (pi / 5) origin Nothing None s


uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d


-- |  Scale deltas between hold and release coordinates by this number.
dragFactor :: Double
dragFactor = pi / 180


-- | Change distance by this amount per one mouse wheel step.
zoomFactor :: Double
zoomFactor = 0.1


-- | Handle mouse clicks to enter drag mode and mouse wheel to zoom.
handleEvent :: World -> Key -> KeyState -> Position -> World
handleEvent w (Char 'r') Down _ =
  w{ target = origin
   , yaw = 0
   , pitch = 0
   , dist = initialDistance
   }
handleMouseEvent w LeftButton Down p =
  w{dragStart = Just p, mode = Rotate}
handleMouseEvent w RightButton Down p =
  w{dragStart = Just p, mode = Pan}
handleMouseEvent w _ Up _ =
  w{dragStart = Nothing}
handleMouseEvent w WheelDown _ _ =
  w{dist = dist w + zoomFactor}
handleMouseEvent w WheelUp _ _ =
  w{dist = dist w - zoomFactor}
handleMouseEvent w _ _ _ = w

-- | Handle mouse movement in drag mode to change pitch & yaw.
handleMovement :: Position -> World -> World
handleMovement p@(Position x y) w =
  case dragStart w of
    Nothing -> w
    Just (Position u v) ->
        let
            xdelta = fromIntegral (x - u) * dragFactor
            ydelta = fromIntegral (y - v) * dragFactor
        in
          case mode w of
            Rotate -> w{ dragStart = Just p
                       , yaw = yaw w - xdelta
                       , pitch = pitch w + ydelta
                       }
            Pan ->
                let
                    !(_, sX, sY) = buildCartesian (yaw w) (pitch w)
                in
                  w{ dragStart = Just p
                   , target = target w <+> (sX .^ xdelta) <-> (sY .^ ydelta)
                   }
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
programName = "csg-raycaster " ++ showVersion version

sizeFromSz2 :: Sz2 -> G.Size
sizeFromSz2 (Sz2 m n) = Size (fromIntegral n) (fromIntegral m)


drawWorld :: Int -> Int -> World -> IO ()
drawWorld width height world =
  let
    makePixel :: World -> Ix2 -> Word8
    !midX = fromIntegral (width `div` 2)
    !midY = fromIntegral (height `div` 2)
    makePixel !w (y :. x) =
            let
                !d = dist w
                !(n, sX, sY) = buildCartesian (yaw w) (pitch w)
                !p = n .^ (-d) <+> target w
                ray :: Ray
                !ray = Ray (p
                            <+> (sX .^ (fromIntegral (x - midX) * 0.7))
                            <+> (sY .^ (fromIntegral (y - midY) * 0.7)), n)
            in
              case ray `cast` solid world of
                S.Just (HitPoint _ (S.Just hn)) ->
                  v
                  where
                    -- TODO blend bright & dark
                    v = round $ 255 * factor
                    factor = abs $ invert n .* hn
                _ ->
                  v
                    where
                      v = fromIntegral $ x + y
    {-# INLINE makePixel #-}
    makePixels :: A.Matrix D Word8
    makePixels = A.makeArray Par wSz (makePixel world)
    wSz = Sz2 width height
  in do
    mArr <- newMArray' wSz
    computeInto mArr makePixels
    A.withPtr mArr $ \ptr -> drawPixels (sizeFromSz2 (sizeOfMArray mArr)) (PixelData Luminance UnsignedByte ptr)
    flush


-- | Read solid def and program arguments, run the actual caster on
-- success.
main :: IO ()
main = do
    _ <- getArgsAndInitialize
    Options{..} <- options (fromString programName) optParser
    solid <-
      case geoFile of
        Nothing -> Right <$> generate arbitrary
        Just fp -> parseGeometryFile (encodeString fp)
    case solid of
        Right b -> do
          worldCell <- newTVarIO (start b)
          putStrLn $ "Rendering " <> show b
          _w <- createWindow programName
          displayCallback $= clear [ColorBuffer] -- TODO Why?
          rowAlignment Unpack $= 1 -- TODO What is this and why do we need this?
          drawWorld width height =<< readTVarIO worldCell
          mouseCallback $= Just (\k ks pos -> do
                                    w <- readTVarIO worldCell
                                    let newWorld = handleMouseEvent w k ks pos
                                    print (newWorld, pos)
                                    atomically $ writeTVar worldCell newWorld)
          mainLoop
            -- (uncurry4 makeColor brightRGBA)
            -- (uncurry4 makeColor darkRGBA)
        Left e -> error $ "Problem when reading solid definition: " ++ e

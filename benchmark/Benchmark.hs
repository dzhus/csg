{-# LANGUAGE BangPatterns #-}

{-
  Single-threaded benchmark for CSG operations.
-}

import Criterion.Main
import Data.Vector.Storable as VS hiding ((++))

import Data.Vec3 hiding (distance)
import Data.CSG


-- | Pixels in meter at unit distance.
resolution :: Double
resolution = 50.0


-- | Build cartesian axes from yaw and pitch with 0 roll. Angles are
-- in radians.
buildCartesian :: Double -> Double -> (SVec3, SVec3, SVec3)
buildCartesian yaw pitch = (u, v, w)
    where u = fromXYZ (cos yaw * cos pitch, sin yaw * cos pitch, sin pitch)
          v = fromXYZ (- (sin yaw), cos yaw, 0)
          w = u >< v
{-# INLINE buildCartesian #-}


-- | Generate initial points of rays within a square region on a plane
-- parallel to YZ.
generateRayPoints :: Int
                  -- ^ Total ray count.
                  -> Double
                  -- ^ Distance of a plane from the origin.
                  -> VS.Vector SVec3
generateRayPoints rayCount distance =
    let
        !(n, sX, sY) = buildCartesian 0 0
        !p = n .^ (-distance)
        -- Find a dimension of a square viewport used to generate
        -- rays. The dimension will most closely fit the required
        -- particle count number without exceeding it
        dim :: Int
        !dim = floor $ (sqrt $ fromIntegral rayCount :: Double)
        !halfDim = dim `div` 2
        -- This differs from Raycaster module by a (fromIntegral dim)
        -- term because range for ray index values are integers (as
        -- opposed to (-1,1) used by gloss)
        !scale = (fromIntegral halfDim) * distance /
                 (resolution * fromIntegral dim)
        -- Initial point of I-th ray on a plane, row-major. 0 ray
        -- starts at (-halfDim, -halfDim).
        ithRay i =
            p <+> (sX .^ (scale * rx))
              <+> (sY .^ (scale * ry))
            where
              (y, x) = i `divMod` dim
              rx = fromIntegral $ x - halfDim
              ry = fromIntegral $ y - halfDim
        {-# INLINE ithRay #-}
    in
      VS.generate (dim * dim) ithRay


-- | Test how long it takes to generate a vector of initial points for
-- rays using 'generateRayPoints'.
testGen :: Int -> Double -> IO ()
testGen rayCount distance = do
  let !_ = generateRayPoints rayCount distance
  return ()


-- | Test raycasting performance for a body.
test :: VS.Vector SVec3
     -- ^ Initial points of test rays. Test rays are directed along
     -- the Ox axis.
     -> Body
     -> IO ()
test rayPoints body =
    let
        !(n, _, _) = buildCartesian 0 0
        posToTrace pos =
            -- Only head of trace gets evaluated
            case trace body (Ray (pos, n)) of
              [] -> False
              _  -> True
    in do
      let !_ = VS.map posToTrace rayPoints
      return ()


rays :: Int
rays = 1000 * 1000

dist :: Double
dist = 100


body1 :: Body
body1 = intersect
        (plane origin (fromXYZ (0, -0.5, 1)))
        (sphere origin 2.5)


body2 :: Body
body2 = intersect
        (cylinder origin (fromXYZ (1, 0, 0)) 4)
        (intersect
         (cylinder origin (fromXYZ (0, 1, 0)) 4)
         (cylinder origin (fromXYZ (0, 0, 1)) 4))


main :: IO ()
main = defaultMain
       [ bgroup "Misc"
         [ bench ("Test rays generation (" ++ show rays ++ ")" ) $
           testGen rays dist
         ]
       , bgroup "Raycasting"
         [ bench "body1" $ test rs body1
         , bench "body2" $ test rs body2
         ]
       ]
       where
         !rs = generateRayPoints rays dist

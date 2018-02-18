{-# LANGUAGE BangPatterns #-}

{-|

Single-threaded benchmark for CSG operations.

-}

import Criterion.Main
import Data.Vector.Storable as V hiding ((++))

import Data.CSG
import qualified Data.Strict.Maybe as S
import Data.Vec3 hiding (Vec3, distance)

-- | Pixels in meter at unit distance.
resolution :: Double
resolution = 50.0


-- | Build cartesian axes from yaw and pitch with 0 roll. Angles are
-- in radians.
buildCartesian :: Double -> Double -> (Vec3, Vec3, Vec3)
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
                  -> V.Vector Vec3
generateRayPoints rayCount distance =
    let
        !(n, sX, sY) = buildCartesian 0 0
        !p = n .^ (-distance)
        -- Find a dimension of a square viewport used to generate
        -- rays. The dimension will most closely fit the required
        -- particle count number without exceeding it
        dim :: Int
        !dim = floor (sqrt $ fromIntegral rayCount :: Double)
        !halfDim = dim `div` 2
        -- This differs from Raycaster module by a (fromIntegral dim)
        -- term because range for ray index values are integers (as
        -- opposed to (-1,1) used by gloss)
        !scale = fromIntegral halfDim * distance /
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
      V.generate (dim * dim) ithRay


-- | Test raycasting performance for a solid.
test :: V.Vector Vec3
     -- ^ Initial points of test rays. Test rays are directed along
     -- the Ox axis.
     -> Solid
     -> V.Vector Bool
test rayPoints solid =
    let
        !(n, _, _) = buildCartesian 0 0
        posToTrace pos =
            -- Only head of trace gets evaluated
            case cast (Ray (pos, n)) solid of
              S.Nothing -> False
              _         -> True
    in
      V.map posToTrace rayPoints


rays :: Int
rays = 1000 * 1000

dist :: Double
dist = 100


solid1 :: Solid
solid1 = plane origin (fromXYZ (0, -0.5, 1))
        `intersect`
        sphere origin 2.5


solid2 :: Solid
solid2 = cylinder origin (fromXYZ (1, 0, 0)) 4
         `intersect`
         cylinder origin (fromXYZ (0, 1, 0)) 4
         `intersect`
         cylinder origin (fromXYZ (0, 0, 1)) 4


main :: IO ()
main = defaultMain
       [ bgroup "Misc"
         [ bench ("Test rays generation (" ++ show rays ++ ")" ) $
           whnf (uncurry generateRayPoints) (rays, dist)
         ]
       , bgroup "Raycasting"
         [ bench "solid1" $ whnf (uncurry test) (rs, solid1)
         , bench "solid2" $ whnf (uncurry test) (rs, solid2)
         ]
       ]
       where
         !rs = generateRayPoints rays dist

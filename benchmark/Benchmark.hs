{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
import Criterion.Main
import Data.Vector.Storable as VS

import Data.Vec3
import Data.CSG


body1 :: Body
body1 = intersect
        (sphere (fromXYZ (0, 0, 0)) 2.5)
        (cylinder (fromXYZ (0, 0, 0)) (fromXYZ (0, 0, 1)) 2)



-- | Pixels in meter at unit distance.
resolution :: Double
resolution = 50.0


-- Generate initial points of rays.
generateRayStarts :: Int
                  -- ^ Total ray count.
                  -> Double
                  -- ^ Distance of plane from origin.
                  -> VS.Vector SVec3
generateRayStarts rayCount distance =
    let
        !(n, sX, sY) = buildCartesian 0 0
        p = n .^ (-distance)
        -- Find a dimension of a square viewport used to generate
        -- rays. The dimension will most closely fit the required
        -- particle count number without exceeding it
        dim :: Int
        dim = floor $ sqrt $ fromIntegral rayCount
        halfDim = dim `div` 2
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
              !(y, x) = i `divMod` dim
              rx = fromIntegral $ x - halfDim
              ry = fromIntegral $ y - halfDim
        {-# INLINE ithRay #-}
    in
      VS.generate (dim * dim) ithRay


test :: Int -> Body -> IO ()
test rayCount body =
    let
        !(n, _, _) = buildCartesian 0 0
        posToTrace pos =
            case trace body (Ray (pos, n)) of
              [] -> False
              _  -> True
    in do
      let !v = VS.map posToTrace $ generateRayStarts rayCount 10
      return ()


main :: IO ()
main = defaultMain
       [ bgroup "Raycasting"
         [
          bench "1M" $ test 1000000 body1
         ]
       ]

-- | Build cartesian axes from yaw and pitch with 0 roll. Angles are
-- in radians.
buildCartesian :: Double -> Double -> (SVec3, SVec3, SVec3)
buildCartesian yaw pitch = (u, v, w)
    where u = fromXYZ (cos yaw * cos pitch, sin yaw * cos pitch, sin pitch)
          v = fromXYZ (- (sin yaw), cos yaw, 0)
          w = u >< v
{-# INLINE buildCartesian #-}

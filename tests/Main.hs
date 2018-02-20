{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.ByteString.Char8 as B
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.CSG
import Data.CSG.Parser

tests :: [TestTree]
tests =
  [ testCase "Parsing cube.geo" $ do
      f <- B.readFile "examples/cube.geo"
      let box = cuboid (fromXYZ (-15, -15, -15)) (fromXYZ (15, 15, 15))
          rounded = sphere origin 20 `intersect` box
          cyl1 = cylinderFrustum (fromXYZ (-16,  0,   0)) (fromXYZ (16, 0, 0)) 10
          cyl2 = cylinderFrustum (fromXYZ (0,  -16,   0)) (fromXYZ (0,  16, 0)) 10
          cyl3 = cylinderFrustum (fromXYZ (0,    0, -16)) (fromXYZ (0,  0, 16)) 10
          cross = cyl1 `unite` (cyl2 `unite` cyl3)
          cutout = complement cross
          top = rounded `intersect` cutout
      parseGeometry f @=? Right top
  , testProperty "CSG complement membership"
    (\(b :: Solid) (p :: Point) ->
       p `inside` b == not (p `inside` complement b))
  ]

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

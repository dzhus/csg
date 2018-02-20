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
      let box = cuboid (fromXYZ (-150, -150, -150)) (fromXYZ (150, 150, 150))
          rounded = sphere origin 200 `intersect` box
          cyl1 = cylinderFrustum
            (fromXYZ (-160, 0, 0)) (fromXYZ (160, 0, 0)) 100
          cyl2 = cylinderFrustum
            (fromXYZ (0, -160, 0)) (fromXYZ (0, 160, 0)) 100
          cyl3 = cylinderFrustum
            (fromXYZ (0, 0, -160)) (fromXYZ (0, 0, 160)) 100
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

{-# LANGUAGE BangPatterns #-}

module Graphics.CSG.Util
    ( SquareRoots
    , solveq
    )

where

import Prelude hiding (Just, Nothing, Maybe)

import Data.Strict.Maybe
import Data.Strict.Tuple

-- | Results of solving a quadratic equation.
type SquareRoots = Maybe (Pair Double Double)


-- | Solve quadratic equation @ax^2 + bx + c = 0@.
--
-- If less than two roots exist, Nothing is returned.
solveq :: Double
       -- ^ a
       -> Double
       -- ^ b
       -> Double
       -- ^ c
       -> SquareRoots
solveq !a !b !c
    | (d > 0)   = Just $ min r1 r2 :!: max r1 r2
    | otherwise = Nothing
    where
      d  =   b * b - 4 * a * c
      q  =   sqrt d
      t  =   2 * a
      r  = - b / t
      s  =   q / t
      r1 =   r - s
      r2 =   r + s
{-# INLINE solveq #-}

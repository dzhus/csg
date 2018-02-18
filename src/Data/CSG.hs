{-# LANGUAGE BangPatterns #-}

{-|

Ray-casting routines for constructive solid geometry.

This module provides constructors for complex solids as well as
membership predicates and routines to compute intersections of such
solids with a ray.

-}

module Data.CSG
    ( -- * Solids
      Solid
    -- ** Primitives
    , plane
    , sphere
    , cylinder
    , cone
    -- ** Complex bodies
    --
    -- | These are made of several primitives, but it's very
    -- convenient to use them in practice.
    , cuboid
    , coneFrustum
    , cylinderFrustum
    -- ** Compositions
    , intersect
    , unite
    , complement
    -- * Ray casting
    , Point
    , Vec3
    , Ray(..)
    , HitPoint(..)
    , HitSegment
    , Trace
    , trace
    -- * Solid membership
    , inside
    )

where

import Prelude hiding (Just, Nothing, Maybe)

import Data.Strict.Maybe
import Data.Strict.Tuple

import Data.Vec3 hiding (Vec3, Matrix)
import qualified Data.Vec3 as V3

import Data.CSG.Util


type Vec3 = SVec3
type Matrix = V3.Matrix SVec3
type Point = SVec3


-- | A ray described by an equation @p(t) = p_0 + v * t@ with an
-- initial point @p_0@ and a direction @v@. Substituting a specific
-- time @t'@ in the equation yields a position of a member point
-- @p(t')@ of the ray.
newtype Ray = Ray (Point, Vec3)


-- | Time at which a ray intersects a surface, with an outward normal
-- to the surface at the hit point. If hit is in infinity, then normal
-- is Nothing.
data HitPoint = HitPoint !Double (Maybe Vec3)
                deriving (Eq, Show)
-- Note that this datatype is strict only on first argument: we do not
-- compare normals when classifying traces and thus do not force
-- calculation of normals.


instance Ord HitPoint where
    compare (HitPoint t1 _) (HitPoint t2 _) = compare t1 t2


-- | A segment of ray inside a solid.
type HitSegment = (Pair HitPoint HitPoint)


-- | Trace of a ray on a solid is a list of time segments/intervals
-- corresponding to portions of ray inside the solid.
--
-- >                       # - ray
-- >                        \
-- >                         \
-- >                          o------------
-- >                      ---/ *           \---
-- >                    -/      *              \-
-- >                   /         *               \
-- >                  (           *  - trace      )
-- >            solid - \           *             /
-- >                    -\          *          /-
-- >                      ---\       *     /---
-- >                          --------o----
-- >                                   \
-- >                                    \
-- >                                    _\/
-- >                                      \
--
--
-- For example, since a ray intersects a plane only once, a half-space
-- primitive defined by this plane results in a half-interval trace of
-- a ray:
--
-- >                                          /
-- >                                         /
-- >                                        /
-- >              #------------------------o*****************>
-- >              |                       /                  |
-- >             ray                     /            goes to infinity
-- >                                    /
-- >                                   /
-- >                                  /
-- >                                 / - surface of half-space
--
-- Ends of segments or intervals are calculated by intersecting the
-- ray and the surface of the primitive. This is done with the help of
-- 'trace', which substitutes the equation of ray @p(t) = p_o + v*t@
-- into the equation which defines the surface and solves it for @t@.
-- If the solid is a composition, traces from primitives are then
-- classified according to operations used to define the solid (union,
-- intersection or complement).
--
-- Although only convex primitives are used in current implementation,
-- compositions may result in concave solids, which is why trace is
-- defined as a list of segments.
--
-- In this example, solid is an intersection of a sphere and a sphere
-- complement:
--
-- >                                /|\
-- >                                 |
-- >                                 |
-- >                                 |
-- >                   -----------   |
-- >              ----/           \--o-
-- >            -/                   * \-
-- >          -/               hs2 - *   \
-- >        -/                       * ---/
-- >       /                         o/
-- >      /                        -/|
-- >     /                        /  |
-- >     |                       /   |
-- >    /                        |   |
-- >    |                       /    |
-- >    |                       |    |
-- >    |                       \    |
-- >    \                        |   |
-- >     |                       \   |
-- >     \                        \  |
-- >      \                        -\|
-- >       \                         o\
-- >        -\                       * ---\
-- >          -\               hs1 - *   /
-- >            -\                   * /-
-- >              ----\           /--o-
-- >                   -----------   |
-- >                                 |
-- >                                 |
-- >                                 # - ray
--
-- Full trace contains two segments: @hs1@ and @hs2@.
type Trace = [HitSegment]


-- | IEEE positive infinity.
infinityP :: Double
infinityP = (/) 1 0


-- | Negative infinity.
infinityN :: Double
infinityN = -infinityP


-- | Hit in negative infinity.
hitN :: HitPoint
hitN = HitPoint infinityN Nothing


-- | Hit in positive infinity.
hitP :: HitPoint
hitP = HitPoint infinityP Nothing


-- | CSG solid is a recursive composition of primitive objects or other
-- solids.
data Solid = Plane !Vec3 !Double
          -- ^ Half-space defined by a unit outward normal and a
          -- distance of boundary plane from the origin.
          | Sphere !Vec3 !Double
          -- ^ Sphere defined by a center point and a radius.
          | Cylinder !Vec3 !Point !Double
          -- ^ Infinite circular cylinder defined by a normalized axis
          -- vector, a point on axis and a radius.
          | Cone !Vec3 !Point !Double !Matrix !Double !Double
          -- ^ Cone defined by an inward axis direction, a vertex and
          -- a cosine to the angle h between the axis and the
          -- generatrix.
          --
          -- Additionally, a transformation matrix $n * n - cos^2 h$,
          -- tangent of angle and odelta are stored for intersection
          -- calculations.
          | Union !Solid !Solid
          | Intersection !Solid !Solid
          | Complement !Solid
            deriving Show


-- | A half-space defined by an arbitary point on the boundary plane
-- and an outward normal (not necessarily a unit vector).
plane :: Point -> Vec3 -> Solid
plane p n = Plane nn (p .* nn)
            where
              nn = normalize n


-- | A sphere defined by a center point and a radius.
sphere :: Vec3 -> Double -> Solid
sphere = Sphere


-- | A rectangular cuboid with faces parallel to axes, defined by two
-- opposite vertices.
cuboid :: Point -> Point -> Solid
cuboid p1 p2 =
  plane p1' (fromXYZ (1, 0, 0))
  `intersect`
  plane p1' (fromXYZ (0, 1, 0))
  `intersect`
  plane p1' (fromXYZ (0, 0, 1))
  `intersect`
  plane p2' (fromXYZ (-1, 0, 0))
  `intersect`
  plane p2' (fromXYZ (0, -1, 0))
  `intersect`
  plane p2' (fromXYZ (0, 0, -1))
  where
    (x1, y1, z1) = toXYZ p1
    (x2, y2, z2) = toXYZ p2
    p2' = fromXYZ (min x1 x2, min y1 y2, min z1 z2)
    p1' = fromXYZ (max x1 x2, max y1 y2, max z1 z2)


-- | An infinite circular cylinder defined by two arbitary points on
-- axis and a radius.
cylinder :: Point -> Point -> Double -> Solid
cylinder p1 p2 = Cylinder (normalize $ p2 <-> p1) p1


-- | A finite right circular cylinder defined by two points on its top
-- and bottom and a radius.
cylinderFrustum :: Point -> Point -> Double -> Solid
cylinderFrustum pb pt r =
    plane pt axis
    `intersect`
    plane pb (invert axis)
    `intersect`
    cylinder pb pt r
    where
      axis = pt <-> pb


-- | An infinite right circular cone defined by an outward axis
-- vector, an apex point and an angle between the generatrix and the
-- axis (in degrees, less than 90).
cone :: Vec3 -> Point -> Double -> Solid
cone a o h =
    let
        rads = h * pi / 180
        h' = cos rads
        n = normalize $ invert a
        gamma = diag (-h' * h')
        m = addM (n `vxv` n) gamma
        ta = tan rads
        odelta = n .* o
    in
      Cone n o h' m ta odelta


-- | A conical frustum given by two points on its axis with radii at
-- that points. One of radii may be zero (in which case one of frustum
-- ends will be the apex).
coneFrustum :: (Point, Double) -> (Point, Double) -> Solid
coneFrustum (p1, r1) (p2, r2) =
    let
        -- Direction from pb to pt is towards apex. Corresponding
        -- radii are rb > rt.
        (pb, rb, pt, rt) = if r1 < r2
                           then (p2, r2, p1, r1)
                           else (p1, r1, p2, r2)
        -- Cone axis and frustum height
        gap =  pt <-> pb
        height = norm gap
        axis = normalize gap
        -- Calculate distance from pt to apex.
        dist = if rt == 0
               then 0
               else height / (rb / rt - 1)
        apex = pt <+> (axis .^ dist)
        -- Angle between generatrix and axis
        degs = atan (rb / (dist + norm (pt <-> pb))) * (180 / pi)
    in
      plane pt axis
      `intersect`
      plane pb (invert axis)
      `intersect`
      cone axis apex degs


-- | Intersection of two solids.
intersect :: Solid -> Solid -> Solid
intersect !b1 !b2 = Intersection b1 b2


-- | Union of two solids.
unite :: Solid -> Solid -> Solid
unite !b1 !b2 = Union b1 b2


-- | Complement to a solid (normals flipped).
complement :: Solid -> Solid
complement !b = Complement b


-- | Trace of a ray on a solid.
trace :: Solid -> Ray -> Trace
{-# INLINE trace #-}

trace b@(Plane n d) (Ray (pos, v)) =
    let
        !f = -(n .* v)
    in
      if f == 0
      then
          -- If ray is parallel to plane and is inside, then trace is
          -- the whole timeline.
          [hitN :!: hitP | inside pos b]
      else
          let
              !t = (pos .* n - d) / f
          in
            if f > 0
            then [HitPoint t (Just n) :!: hitP]
            else [hitN :!: HitPoint t (Just n)]

trace (Sphere c r) (Ray (pos, v)) =
      let
          !d = pos <-> c
          !roots = solveq (v .* v) (v .* d * 2) (d .* d - r * r)
          normal !u = normalize (u <-> c)
      in
        case roots of
          Nothing -> []
          Just (t1 :!: t2) ->
              [HitPoint t1 (Just $ normal $ moveBy pos v t1) :!:
               HitPoint t2 (Just $ normal $ moveBy pos v t2)]

trace (Cylinder n c r) (Ray (pos, v)) =
    let
        d = (pos <-> c) >< n
        e = v >< n
        roots = solveq (e .* e) (d .* e * 2) (d .* d - r * r)
        normal u = normalize $ h <-> (n .^ (h .* n))
            where h = u <-> c
    in
      case roots of
        Nothing -> []
        Just (t1 :!: t2) ->
            [HitPoint t1 (Just $ normal $ moveBy pos v t1) :!:
                      HitPoint t2 (Just $ normal $ moveBy pos v t2)]

trace (Cone n c _ m ta odelta) (Ray (pos, v)) =
    let
      delta = pos <-> c
      c2 = dotM v     v     m
      c1 = dotM v     delta m
      c0 = dotM delta delta m
      roots = solveq c2 (2 * c1) c0
      normal !u = normalize $ nx .^ (1 / ta) <-> ny .^ ta
          where h = u <-> c
                -- Component of h parallel to cone axis
                ny' = n .^ (n .* h)
                ny = normalize ny'
                -- Perpendicular component
                nx = normalize $ h <-> ny'
    in
      case roots of
        Nothing -> []
        Just (t1 :!: t2) ->
            let
                pos1 = moveBy pos v t1
                pos2 = moveBy pos v t2
            in
              case ((pos1 .* n - odelta) > 0, (pos2 .* n - odelta) > 0) of
                (True, True) -> [HitPoint t1 (Just $ normal pos1) :!:
                                 HitPoint t2 (Just $ normal pos2)]
                (True, False) -> [hitN :!:
                                  HitPoint t1 (Just $ normal pos1)]
                (False, True) -> [HitPoint t2 (Just $ normal pos2) :!:
                                  hitP]
                (False, False) -> []

trace (Intersection b1 b2) !p =
    intersectTraces tr1 tr2
        where
          tr1 = trace b1 p
          tr2 = trace b2 p

trace (Union b1 b2) !p =
    uniteTraces tr1 tr2
        where
          tr1 = trace b1 p
          tr2 = trace b2 p

trace (Complement b) !p =
    complementTrace $ trace b p


-- | Union of two traces.
uniteTraces :: Trace -> Trace -> Trace
uniteTraces u [] = u
uniteTraces u (v:t2) =
      uniteTraces (unite1 u v) t2
      where
        merge :: HitSegment -> HitSegment -> HitSegment
        merge (a1 :!: b1) (a2 :!: b2) = min a1 a2 :!: max b1 b2
        {-# INLINE merge #-}
        unite1 :: Trace -> HitSegment -> Trace
        unite1 [] hs = [hs]
        unite1 t@(hs1@(a1 :!: b1):tr') hs2@(a2 :!: b2)
            | b1 < a2 = hs1:unite1 tr' hs2
            | a1 > b2 = hs2:t
            | otherwise = unite1 tr' (merge hs1 hs2)
        {-# INLINE unite1 #-}
{-# INLINE uniteTraces #-}


-- | Intersection of two traces.
intersectTraces :: Trace -> Trace -> Trace
intersectTraces tr1 tr2 =
    let
        -- Overlap two overlapping segments
        overlap :: HitSegment -> HitSegment -> HitSegment
        overlap (a1 :!: b1) (a2 :!: b2) = max a1 a2 :!: min b1 b2
        {-# INLINE overlap #-}
    in
      case tr2 of
        [] -> []
        (hs2@(a2 :!: b2):tr2') ->
            case tr1 of
              [] -> []
              (hs1@(a1 :!: b1):tr1') | b1 < a2 -> intersectTraces tr1' tr2
                                     | b2 < a1 -> intersectTraces tr1 tr2'
                                     | otherwise -> overlap hs1 hs2:intersectTraces tr1' tr2
{-# INLINE intersectTraces #-}


-- | Complement to a trace (normals flipped).
complementTrace :: Trace -> Trace
complementTrace ((sp@(HitPoint ts _) :!: ep):xs) =
    start ++ complementTrace' ep xs
    where
      flipNormals :: HitSegment -> HitSegment
      flipNormals (HitPoint t1 n1 :!: HitPoint t2 n2) =
          HitPoint t1 (invert <$> n1) :!: HitPoint t2 (invert <$> n2)
      {-# INLINE flipNormals #-}
      -- Start from infinity if first hitpoint is finite
      start = if isInfinite ts
              then []
              else [flipNormals $ hitN :!: sp]
      complementTrace' :: HitPoint -> Trace -> Trace
      complementTrace' c ((a :!: b):tr) =
          -- Bridge between the last point of the previous segment and
          -- the first point of the next one.
          flipNormals (c :!: a):complementTrace' b tr
      complementTrace' a@(HitPoint t _) [] =
          -- End in infinity if last hitpoint is finite
          [flipNormals (a :!: hitP) | not (isInfinite t)]
complementTrace [] = [hitN :!: hitP]
{-# INLINE complementTrace #-}


-- | True if the point is in inside the solid.
inside :: Point -> Solid -> Bool
{-# INLINE inside #-}

inside !pos (Plane n d) = (pos .* n - d) < 0

inside !pos (Sphere c r) = norm (pos <-> c) < r

inside !pos (Cylinder n c r) =
    norm (h <-> (n .^ (h .* n))) < r
    where
      h = pos <-> c

inside !pos (Cone n c a _ _ _) =
    n .* normalize (pos <-> c) > a

inside !p (Intersection b1 b2) = inside p b1 && inside p b2

inside !p (Union b1 b2) = inside p b1 || inside p b2

inside !p (Complement b) = not $ inside p b


-- | Move point by velocity vector for given time and return new
-- position.
moveBy :: Point
       -- ^ Current position.
       -> Vec3
       -- ^ Velocity.
       -> Double
       -- ^ Time step.
       -> Point
moveBy !p !v !t = p <+> (v .^ t)
{-# INLINE moveBy #-}

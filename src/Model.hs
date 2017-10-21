module Model where

import Vec3
import Vec3 (Vec3)
import Ray
import Ray (Ray)

import Data.Semigroup

data HitRecord = HitRecord {
  t :: Float,
  position :: Vec3,
  normal :: Vec3
} deriving (Show, Eq)

-- HitRecord itself only forms a semigroup, no sensible identity
-- semigroup operator to choose the closer hit
-- the implementation (comparing t) assumes that hits are only compared for the same ray
instance Semigroup HitRecord where
  a <> b = if (t a) < (t b) then a else b

-- Definition for an object that can be hit
-- Any hitable object is a function that takes a ray and bounds, and returns if it it the object
-- within the bounds
type Hitable = (Ray -> Float -> Float -> Maybe HitRecord)

-- Dummy apply function to make usaage more explicit
-- e.g. `hit (sphere...) ray` instead of `(sphere ...) ray`
hit :: Hitable -> Ray -> Float -> Float -> Maybe HitRecord
hit hitable = hitable

withinBound :: Float -> Float -> Float -> Bool
withinBound t tmin tmax = (t < tmax) && (t > tmin)

-- Sphere
-- Surface of sphere with center (cx,cy,cz) defined as
-- (x-cx)^2 + (y-cy)^2 + (z-cz)^2 = radius^2 = dot (p-c) (p-c) where p = (x,y,z)
-- a ray p(t) = A+t*B, solve for t where dot (p(t) - c) (p(t) -c) = radius^2
sphere :: Vec3 -> Float -> Hitable
sphere center radius ray tmin tmax =
    case discriminant of
      _ | discriminant > 0 && (withinBound t1 tmin tmax) -> Just (makeHitRecord t1)
      _ | discriminant > 0 && (withinBound t2 tmin tmax) -> Just (makeHitRecord t2)
      _ -> Nothing
    where
      oc = (origin ray) - center
      rdir = dir ray
      a = dot rdir rdir
      b = dot oc rdir
      c = (dot oc oc) - radius * radius
      discriminant = b*b - a*c
      t1 = (-b - (sqrt discriminant)) / a
      t2 = (-b + (sqrt discriminant)) / a
      makeHitRecord =
        \t ->
          let
            p = pointAtParameter ray t
            normal = (p - center) /: radius
          in
            HitRecord t p normal

-- Hitable List
-- iterate over list, return closest
hitableList :: [Hitable] -> Hitable
hitableList hs ray tmin tmax =
  foldr (\x closest -> (hit x ray tmin tmax) <> closest) Nothing hs


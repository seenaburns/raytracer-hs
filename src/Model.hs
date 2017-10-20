module Model where

import Vec3
import Vec3 (Vec3)
import Ray
import Ray (Ray)

data HitRecord = HitRecord {
  t :: Float,
  position :: Vec3,
  normal :: Vec3
}

type Hitable = Ray -> Float -> Float -> Bool

hitSphere :: Vec3 -> Float -> Hitable
hitSphere center radius ray tmin tmax =
  discriminant > 0
  where
    oc = (origin ray) - center
    rdir = dir ray
    a = dot rdir rdir
    b = 2.0 * (dot oc rdir)
    c = (dot oc oc) - radius * radius
    discriminant = b*b - 4*a*c

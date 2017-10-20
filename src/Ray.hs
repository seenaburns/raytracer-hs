module Ray where

import Vec3
import Vec3 (Vec3)
import qualified Vec3

-- Ray defined as A+t*B, where A=origin, B=dir
-- Dir may not be unit vector
data Ray = Ray {
  origin :: !Vec3,
  dir    :: !Vec3
} deriving (Show, Eq)

-- Return position at t from A + t*B
pointAtParameter :: Ray -> Float -> Vec3
pointAtParameter r t = (origin r) + ((dir r) *: t)

-- Create ray from p1 to p2
rayFromTo :: Vec3 -> Vec3 -> Ray
rayFromTo p1 p2 = Ray p1 (p2-p1)


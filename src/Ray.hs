module Ray where

import Vec3
import Vec3 (Vec3)
import qualified Vec3

-- Ray defined as A+t*B, where A=origin, B=dir
data Ray = Ray {
  origin :: !Vec3,
  dir    :: !Vec3
} deriving (Show, Eq)

-- Return position at t from A + t*B
pointAtParameter :: Ray -> Float -> Vec3
pointAtParameter r t = (origin r) + ((dir r) *: t)

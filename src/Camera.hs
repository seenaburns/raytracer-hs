module Camera where

import Vec3
import Vec3 (Vec3)
import Ray
import Ray (Ray)

data Camera = Camera {
  origin :: Vec3,
  lower_left_corner :: Vec3,
  horizontal :: Vec3,
  vertical :: Vec3
}

makeCamera :: Vec3 -> Vec3 -> Vec3 -> Float -> Float -> Camera
makeCamera lookfrom lookat vup vfov aspect =
  Camera o llc h vert
  where
    theta = vfov * pi / 180
    half_height = tan (theta/2)
    half_width = aspect * half_height
    w = normalized (lookfrom - lookat)
    u = normalized $ cross vup w
    v = cross w u
    o = lookfrom
    llc = o - (u *: half_width) - (v *: half_height) - w
    h = u *: (2*half_width)
    vert = v *: (2*half_height)

getRay :: Camera -> Float -> Float -> Ray
getRay c s t =
  rayFromTo from to
  where
    from = Camera.origin c
    to = (lower_left_corner c) + ((horizontal c) *: s) + ((vertical c) *: t)


module Render where

import Vec3
import Vec3 (Vec3)
import Ray
import Ray (Ray(Ray))
import Model

import Data.Maybe

-- Render module takes a scene and produces a buffer of pixels
-- In the most basic case this iterates over pixels on the image plane and and sends a Ray.

-- Global min/max constants for ray bounds
tmin = 0.00001
tmax = 1000

data Scene = Scene {
  objects :: Hitable,
  imgX    :: Int,
  imgY    :: Int
}

render :: Scene -> [Vec3]
render scene =
  do
    j <- (reverse [0..height-1])
    i <- ([0..width-1])
    let u = (fromIntegral i) / (fromIntegral width)
    let v = (fromIntegral j) / (fromIntegral height)
    let r = Ray origin (lowerleftcorner + (horizontal *: u) + (vertical *: v))
    return $ color (objects scene) r
  where
    width = (imgX scene)
    height = (imgY scene)
    -- Define origin and picture plane from corner, width and height
    lowerleftcorner = vec3 (-2) (-1) (-1)
    horizontal      = vec3 4 0 0
    vertical        = vec3 0 2 0
    origin          = vec3 0 0 0

color :: Hitable -> Ray -> Vec3
color objects r =
  case hr of
    Just h  -> ((normal h) +: 1) *: 0.5
    Nothing -> backgroundColor r
  where
    hr = hit objects r tmin tmax

-- Blend between two colors with the y component of the given ray
backgroundColor :: Ray -> Vec3
backgroundColor r =
  lerp (vec3 0.5 0.7 1.0) (vec3 1 1 1) t
  where
    unitDir = (normalized (dir r))
    t = 0.5 * ((y unitDir) + 1.0)

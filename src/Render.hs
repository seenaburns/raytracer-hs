module Render where

import Vec3
import Vec3 (Vec3)
import Ray
import Ray (Ray(Ray))
import Model

import Data.Maybe
import System.Random
import Control.Monad

-- Render module takes a scene and produces a buffer of pixels
-- In the most basic case this iterates over pixels on the image plane and and sends a Ray.

-- Global min/max constants for ray bounds
tmin = 0.00001
tmax = 1000
nsamples = 100

data Scene = Scene {
  objects :: Hitable,
  imgX    :: Int,
  imgY    :: Int
}

render :: Scene -> IO [Vec3]
render scene =
  sequence $ do
    j <- fmap fromIntegral (reverse [0..(imgY scene)-1])
    i <- fmap fromIntegral ([0..(imgX scene)-1])
    return $ antialias nsamples scene i j

sample :: Scene -> Float -> Float -> Vec3
sample scene i j =
  color (objects scene) r
  where
    -- Define origin and picture plane from corner, width and height
    lowerleftcorner = vec3 (-2) (-1) (-1)
    horizontal      = vec3 4 0 0
    vertical        = vec3 0 2 0
    origin          = vec3 0 0 0
    u = i / (fromIntegral (imgX scene))
    v = j / (fromIntegral (imgY scene))
    r = Ray origin (lowerleftcorner + (horizontal *: u) + (vertical *: v))

randomFloat :: IO Float
randomFloat = randomRIO (0.0,1.0)

antialias :: Int -> Scene -> Float -> Float -> IO Vec3
antialias ns scene i j =
  fmap (\s -> (foldr (+) (vec3 0 0 0) s) /: (fromIntegral ns)) samples
  where
    runSample =
      do
        a <- randomFloat
        b <- randomFloat
        let ii = i + a
        let jj = j + b
        return $ sample scene ii jj
    samples = replicateM ns runSample

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

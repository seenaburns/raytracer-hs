module Util where

import Vec3

import Control.Monad
import Control.Monad.State
import System.Random

type RandomState a = State StdGen a

randomFloat :: RandomState Float
randomFloat = state $ randomR (0.0,1.0)

randomVec3 :: RandomState Vec3
randomVec3 =
  do
    x <- randomFloat
    y <- randomFloat
    z <- randomFloat
    return $ vec3 x y z

-- repeatedly generate vec3 until one with length less than 1
randomVec3InUnitSphere :: RandomState Vec3
randomVec3InUnitSphere =
  do
    v <- randomVec3
    let v' = (v *: 2) - (vec3 1 1 1)
    if ((dot v' v') >= 1.0)
      then randomVec3InUnitSphere
      else return $ v'

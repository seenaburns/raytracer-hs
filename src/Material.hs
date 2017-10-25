module Material where

import Util
import Model
import Ray
import Ray (Ray)
import Vec3
import Vec3 (Vec3)

data MaterialResponse = MaterialResponse {
  attenuation :: Vec3,
  scatterRay  :: Ray
}

type Material = (Ray -> HitRecord -> RandomState (Maybe MaterialResponse))

lambertian :: Vec3 -> Material
lambertian albedo ray hr =
  do
    v <- randomVec3InUnitSphere
    let target = (position hr) + (normal hr) + v
    let scatter = rayFromTo (position hr) target
    let attenuation = albedo
    return $ Just (MaterialResponse attenuation scatter)

metal :: Vec3 -> Float -> Material
metal albedo fuzz ray hr =
  do
    v <- randomVec3InUnitSphere
    let reflected = reflect (normalized (dir ray)) (normal hr)
    let scatter = Ray (position hr) (reflected + (v *: fuzz))
    let attenuation = albedo
    return $ Just (MaterialResponse attenuation scatter)

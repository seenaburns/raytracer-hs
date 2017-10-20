import Vec3
import Vec3 (Vec3)
import qualified IO
import Ray
import Ray (Ray(Ray))
import Model

nx = 200
ny = 100
tmin = 0.00001
tmax = 1000

main :: IO ()
main = putStr $ IO.bufToPPM nx ny (testScene nx ny)

-- Test Scene
testScene :: Int -> Int -> [Vec3]
testScene width height =
  do
    j <- (reverse [0..height-1])
    i <- ([0..width-1])
    let u = (fromIntegral i) / (fromIntegral width)
    let v = (fromIntegral j) / (fromIntegral height)
    let r = Ray origin (lowerleftcorner + (horizontal *: u) + (vertical *: v))
    return $ color r
  where
    -- Define origin and picture plane from corner, width and height
    lowerleftcorner = vec3 (-2) (-1) (-1)
    horizontal      = vec3 4 0 0
    vertical        = vec3 0 2 0
    origin          = vec3 0 0 0

color :: Ray -> Vec3
color r =
  if (hit (sphere (vec3 0 0 (-1)) 0.5) r tmin tmax)
    then (vec3 1 0 0)
    else backgroundColor r

-- Blend between two colors with the y component of the given ray
backgroundColor :: Ray -> Vec3
backgroundColor r =
  lerp (vec3 0.5 0.7 1.0) (vec3 1 1 1) t
  where
    unitDir = (normalized (dir r))
    t = 0.5 * ((y unitDir) + 1.0)
